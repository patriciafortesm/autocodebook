# =============================================================================
# autocodebook — Arvore de fluxo (CONSORT composavel)
# =============================================================================
# Modelo de dados: uma arvore de nos guardada em .cb_env$flow.
#
# Tipos de no:
#   - "root"    : coorte inicial (1, criado automaticamente)
#   - "step"    : reducao linear (filtro). 1 filho.
#   - "split"   : ramificacao por coluna. N filhos (um por categoria).
#   - "outcome" : folha empilhada (contagem de desfecho). Sem filhos.
#
# A arvore e construida incrementalmente:
#   track_split(by=)     -> ramifica TODAS as folhas-corrente pela coluna
#   track_outcomes(vars=)-> anexa contagens de desfecho nas folhas-corrente
#
# "Folhas-corrente" = os nos no nivel mais profundo que ainda podem ramificar
# (root no inicio; depois os filhos do ultimo split).
#
# REGRA BIG DATA: cada split/outcome roda group_by no Spark; so os N
# agregados (poucas linhas) voltam pro R.
# =============================================================================


# Inicializa a arvore no environment (chamado por cb_init via flow_reset)
.flow_init <- function() {
  .cb_env$flow <- list(
    id_col   = .cb_env$id_col,
    n_root   = NA_integer_,     # N da coorte inicial
    levels   = list(),          # lista de niveis de split
    outcomes = list(),          # desfechos empilhados (por folha)
    built    = FALSE
  )
  invisible(NULL)
}

#' Reset the flow tree
#'
#' Clears the CONSORT flow tree. Called automatically by `cb_init()`.
#' @return Invisible NULL.
#' @export
flow_reset <- function() {
  .flow_init()
  invisible(NULL)
}

#' Get the current flow tree (raw structure)
#'
#' Returns the internal flow representation. Mostly for debugging /
#' programmatic access. For a tidy table, use `flow_table()`.
#' @return A list describing the flow tree.
#' @export
flow_get <- function() {
  if (is.null(.cb_env$flow)) .flow_init()
  .cb_env$flow
}

# -----------------------------------------------------------------------------
# Helper: conta N unico de individuos (ou linhas se assume_unique)
# -----------------------------------------------------------------------------

.flow_count_ids <- function(sdf, assume_unique = FALSE) {
  id_col <- .cb_env$id_col
  if (inherits(sdf, "tbl_spark")) {
    if (isTRUE(assume_unique)) {
      as.integer(sparklyr::sdf_nrow(sdf))
    } else {
      sdf %>% dplyr::select(dplyr::all_of(id_col)) %>%
        dplyr::distinct() %>% sparklyr::sdf_nrow() %>% as.integer()
    }
  } else {
    if (isTRUE(assume_unique)) nrow(sdf)
    else length(unique(sdf[[id_col]]))
  }
}

# -----------------------------------------------------------------------------
# Helper: conta N por categoria de uma coluna (group_by no Spark)
# Retorna tibble (categoria, n_ids) ja coletada.
# -----------------------------------------------------------------------------

.flow_count_by <- function(sdf, by_col) {
  id_col <- .cb_env$id_col
  b <- rlang::sym(by_col)

  # Conta individuos unicos por categoria.
  # Usa coalesce pra agrupar NA numa categoria "(NA)".
  res <- sdf %>%
    dplyr::mutate(.cat_v = dplyr::coalesce(as.character(!!b), "(NA)")) %>%
    dplyr::group_by(.cat_v) %>%
    dplyr::summarise(
      n_ids = dplyr::n_distinct(!!rlang::sym(id_col)),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::rename(categoria = .cat_v) %>%
    dplyr::arrange(categoria)

  res
}

# -----------------------------------------------------------------------------
# Helper: conta desfecho (quantos têm outcome == 1 / TRUE) por categoria
# Para outcome binario: conta os "positivos". Retorna n e pct.
# -----------------------------------------------------------------------------

.flow_count_outcome <- function(sdf, outcome_col, split_cols = character()) {
  id_col <- rlang::sym(.cb_env$id_col)
  o <- rlang::sym(outcome_col)

  # Agrupa pelos splits acumulados + conta positivos do desfecho
  grp_syms <- lapply(split_cols, rlang::sym)

  base <- sdf %>%
    dplyr::mutate(
      .is_pos = dplyr::if_else(
        as.numeric(!!o) == 1 | as.logical(!!o) == TRUE, 1L, 0L
      )
    )

  if (length(split_cols) > 0) {
    base <- base %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(split_cols),
                                  ~ dplyr::coalesce(as.character(.x), "(NA)")))
    base <- base %>% dplyr::group_by(!!!grp_syms)
  }

  out <- base %>%
    dplyr::summarise(
      n_total = dplyr::n_distinct(!!id_col),
      n_pos   = sum(.is_pos, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::collect()

  out
}

# =============================================================================
# track_split() — adiciona um nivel de ramificacao
# =============================================================================

#' Split the cohort into branches by a column (CONSORT flowchart)
#'
#' Adds one branching level to the flow tree. The cohort is divided by the
#' distinct values of `by`. Chain multiple `track_split()` calls to create
#' nested branches (e.g. exposure then mediator). Passes the data through
#' unchanged, so it fits in a `%>%` pipeline.
#'
#' @param sdf A Spark DataFrame or local data frame.
#' @param by Character. Column name to split by. Its distinct values become
#'   the branches. NA values are grouped as "(NA)".
#' @param label Optional character. A human-readable name for this split
#'   level (e.g. "Exposure: drought"). Defaults to `by`.
#' @param value_labels Optional named character vector mapping raw values to
#'   readable labels, e.g. `c("0" = "Sem seca", "1" = "Com seca")`. If not
#'   given, the function tries factor levels / labelled attributes on the
#'   column; failing that, uses the raw value.
#' @param max_levels Integer. Safety cap on nesting depth. Default 3.
#'
#' @return `sdf` unchanged (for piping).
#' @export
#'
#' @examples
#' \dontrun{
#' df %>%
#'   track_split(by = "exposto_seca", label = "Exposição: seca",
#'               value_labels = c("0" = "Sem seca", "1" = "Com seca")) %>%
#'   track_split(by = "migrou", label = "Mediador: migração",
#'               value_labels = c("0" = "Não migrou", "1" = "Migrou")) %>%
#'   track_outcomes(c("obito_dcv", "obito_infec"))
#' }
track_split <- function(sdf, by, label = NULL, value_labels = NULL,
                        max_levels = 3L) {
  if (is.null(.cb_env$flow)) .flow_init()

  cols <- if (inherits(sdf, "tbl_spark")) colnames(sdf) else names(sdf)
  if (!by %in% cols) {
    stop("[autocodebook] Coluna '", by, "' nao encontrada nos dados.",
         call. = FALSE)
  }

  n_levels <- length(.cb_env$flow$levels)
  if (n_levels >= max_levels) {
    warning("[autocodebook] Limite de ", max_levels,
            " niveis de split atingido. Split por '", by, "' ignorado.",
            call. = FALSE)
    return(invisible(sdf))
  }

  # Auto-deteccao de value_labels se nao fornecido (fatores em df local)
  if (is.null(value_labels) && !inherits(sdf, "tbl_spark")) {
    col_data <- sdf[[by]]
    if (is.factor(col_data)) {
      lv <- levels(col_data)
      value_labels <- stats::setNames(lv, lv)
    }
  }

  # N da raiz (so na primeira vez)
  if (is.na(.cb_env$flow$n_root)) {
    .cb_env$flow$n_root <- .flow_count_ids(sdf)
  }

  # Colunas de split acumuladas (para group_by aninhado)
  prev_cols <- vapply(.cb_env$flow$levels, function(l) l$by, character(1))
  all_cols  <- c(prev_cols, by)

  # Conta N por combinacao de todos os niveis ate aqui
  id_col <- rlang::sym(.cb_env$id_col)
  grp_syms <- lapply(all_cols, rlang::sym)

  counts <- sdf %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(all_cols),
                                ~ dplyr::coalesce(as.character(.x), "(NA)"))) %>%
    dplyr::group_by(!!!grp_syms) %>%
    dplyr::summarise(n_ids = dplyr::n_distinct(!!id_col), .groups = "drop") %>%
    dplyr::collect()

  .cb_env$flow$levels[[length(.cb_env$flow$levels) + 1L]] <- list(
    by           = by,
    label        = if (!is.null(label)) label else by,
    value_labels = value_labels,
    counts       = counts,
    cols         = all_cols
  )

  .cb_msg("[autocodebook] split por '", by, "' -> ",
          nrow(counts), " grupos (nivel ", n_levels + 1L, ")")

  invisible(sdf)
}

# Helper: aplica value_labels a um valor cru
.flow_value_label <- function(raw, value_labels) {
  if (is.null(value_labels)) return(as.character(raw))
  key <- as.character(raw)
  if (key %in% names(value_labels)) value_labels[[key]] else key
}

# =============================================================================
# track_outcomes() — empilha desfechos nas folhas correntes
# =============================================================================

#' Attach outcome counts to the current leaves (CONSORT flowchart)
#'
#' Adds one or more outcome variables, counted within each current leaf
#' (combination of all splits so far). Outcomes are stacked, not branched.
#' Each outcome is treated as binary: counts how many individuals have
#' value == 1 (or TRUE), plus the percentage within the leaf.
#'
#' @param sdf A Spark DataFrame or local data frame.
#' @param vars Character vector of outcome column names (binary 0/1 or
#'   logical).
#' @param labels Optional named list (var -> label).
#'
#' @return `sdf` unchanged (for piping).
#' @export
track_outcomes <- function(sdf, vars, labels = NULL) {
  if (is.null(.cb_env$flow)) .flow_init()

  cols <- if (inherits(sdf, "tbl_spark")) colnames(sdf) else names(sdf)
  miss <- setdiff(vars, cols)
  if (length(miss) > 0) {
    stop("[autocodebook] Colunas de desfecho nao encontradas: ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  # Colunas de split acumuladas (define as folhas)
  split_cols <- if (length(.cb_env$flow$levels) > 0) {
    .cb_env$flow$levels[[length(.cb_env$flow$levels)]]$cols
  } else character()

  # N da raiz se ainda nao setado (caso outcomes sem split)
  if (is.na(.cb_env$flow$n_root)) {
    .cb_env$flow$n_root <- .flow_count_ids(sdf)
  }

  for (v in vars) {
    cnt <- .flow_count_outcome(sdf, v, split_cols = split_cols)
    lbl <- if (!is.null(labels) && !is.null(labels[[v]])) labels[[v]] else v
    .cb_env$flow$outcomes[[length(.cb_env$flow$outcomes) + 1L]] <- list(
      var    = v,
      label  = lbl,
      counts = cnt,           # tibble: split_cols + n_total + n_pos
      cols   = split_cols
    )
    .cb_msg("[autocodebook] desfecho '", v, "' contado em ",
            max(1L, nrow(cnt)), " folha(s)")
  }

  invisible(sdf)
}

# =============================================================================
# flow_table() — converte a arvore numa tabela tidy (para export editavel)
# =============================================================================

#' Flow tree as a tidy table
#'
#' Flattens the CONSORT flow tree into a publication-friendly data frame.
#' One row per leaf x outcome. Split levels become named columns (using their
#' labels), values are mapped through value_labels, and percentages are
#' formatted as readable strings.
#'
#' @return A tibble.
#' @export
flow_table <- function() {
  fl <- flow_get()
  if (is.na(fl$n_root)) {
    return(tibble::tibble())
  }

  n_levels <- length(fl$levels)

  # Funcao: dado um data frame com as colunas de split cruas, devolve
  # um data frame com colunas nomeadas pelos labels dos niveis e valores
  # mapeados pelos value_labels.
  .label_split_cols <- function(df) {
    if (n_levels == 0) return(df[, character(0), drop = FALSE])
    out <- list()
    for (lv in seq_len(n_levels)) {
      lvl  <- fl$levels[[lv]]
      raw  <- as.character(df[[lvl$by]])
      mapped <- vapply(raw, .flow_value_label, character(1),
                       value_labels = lvl$value_labels)
      out[[lvl$label]] <- mapped
    }
    as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  }

  # Caso sem splits e sem outcomes: so a raiz
  if (n_levels == 0 && length(fl$outcomes) == 0) {
    return(tibble::tibble(Grupo = "Coorte", N = fl$n_root))
  }

  # Caso com splits mas sem outcomes: tabela de N por folha
  if (length(fl$outcomes) == 0) {
    last <- fl$levels[[n_levels]]
    labeled <- .label_split_cols(last$counts)
    labeled$N <- last$counts$n_ids
    return(tibble::as_tibble(labeled))
  }

  # Caso com outcomes: uma linha por folha × desfecho
  rows <- list()
  for (oc in fl$outcomes) {
    cnt <- oc$counts
    labeled <- if (length(oc$cols) > 0) {
      .label_split_cols(cnt)
    } else {
      data.frame(Grupo = rep("Coorte", nrow(cnt)),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }
    labeled[["Desfecho"]] <- oc$label
    labeled[["N total"]]  <- cnt$n_total
    labeled[["N evento"]] <- cnt$n_pos
    pct <- ifelse(cnt$n_total > 0, cnt$n_pos / cnt$n_total, NA_real_)
    labeled[["%"]] <- vapply(pct, .pct_str, character(1))
    rows[[length(rows) + 1L]] <- labeled
  }
  out <- dplyr::bind_rows(rows)
  tibble::as_tibble(out)
}
