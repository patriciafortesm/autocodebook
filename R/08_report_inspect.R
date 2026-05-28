# =============================================================================
# autocodebook — Inspecao de variaveis
# =============================================================================
# Cada variavel vira UM patchwork (combinacao de subplots) montado pelos
# helpers do 07_report_plots.R. Isso reduz a quantidade de figuras no
# relatorio e facilita a leitura.
#
# Output principal:
#   list(
#     plots     = list(<var> = <patchwork>, ...),
#     summaries = list(<var> = <tibble>, ...),
#     extras    = list()                            # reservado
#   )
# =============================================================================

#' @keywords internal
#' @noRd

# -----------------------------------------------------------------------------
# .agg_var_cross  — agrega todas as informacoes que UM painel transversal
# precisa: hist (numeric/date) ou tabela cat (categorical) + sumario
# -----------------------------------------------------------------------------

.agg_var_cross <- function(.data, var, var_class, n_bins = 30, top_n_cat = 20) {

  out <- list(hist = NULL, cat = NULL, summary = NULL)

  if (var_class %in% c("numeric", "integer")) {
    out$hist    <- .agg_histogram_numeric(.data, var, n_bins = n_bins)
    out$summary <- .agg_summary_numeric(.data, var)
  } else if (var_class == "date") {
    d <- .agg_date(.data, var)
    out$hist    <- d$hist
    out$summary <- d$range
  } else if (var_class %in% c("character", "logical")) {
    cat_df      <- .agg_categorical(.data, var, top_n = top_n_cat)
    out$cat     <- cat_df
    out$summary <- cat_df  # usado pelo .panel_stats_text
  }

  out
}

# -----------------------------------------------------------------------------
# .agg_var_long  — agrega informacoes para o painel longitudinal:
# by_time + miss_by_time + intra_id
# -----------------------------------------------------------------------------

.agg_var_long <- function(.data, var, var_class, id_var, time_var,
                          top_n_cat = 8) {

  out <- list(by_time = NULL, miss_by_time = NULL, intra = NULL)

  # By time — depende da classe
  out$by_time <- if (var_class %in% c("numeric", "integer")) {
    .agg_numeric_by_time_quantiles(.data, var, time_var)
  } else if (var_class %in% c("character", "logical")) {
    .agg_categorical_by_time(.data, var, time_var, top_n = top_n_cat)
  } else if (var_class == "date") {
    .agg_date_by_time(.data, var, time_var)
  } else NULL

  # Missingness por tempo (sempre)
  out$miss_by_time <- tryCatch(
    .agg_missingness_by_time(.data, var, time_var),
    error = function(e) NULL
  )

  # Variacao intra-ID (sempre)
  out$intra <- tryCatch(
    .agg_intra_id_variation(.data, var, id_var),
    error = function(e) NULL
  )

  out
}

# -----------------------------------------------------------------------------
# .inspect_variables_cross_sectional()
# -----------------------------------------------------------------------------

.inspect_variables_cross_sectional <- function(.data, variables = NULL,
                                                labels = list(),
                                                id_var = NULL,
                                                n_bins = 30,
                                                top_n_cat = 20,
                                                treat_as_categorical = NULL) {

  all_vars <- if (inherits(.data, "tbl_spark")) colnames(.data) else names(.data)

  if (is.null(variables)) {
    variables <- setdiff(all_vars, id_var)
  } else {
    variables <- intersect(variables, all_vars)
  }

  mini_plots <- list()
  summaries <- list()
  metas <- list()

  for (var in variables) {
    cls <- .infer_data_class(.data, var, id_var = id_var)
    # Override por argumento manual
    if (!is.null(treat_as_categorical) && var %in% treat_as_categorical) {
      cls <- "character"
    }
    if (cls == "id") next
    lbl <- if (!is.null(labels[[var]])) labels[[var]] else var

    mp <- tryCatch({
      # CONSOLIDACAO: calcula cardinalidade (n_distinct + n_total + n_miss +
      # pct_missing) UMA vez por variavel, numa unica varredura. Isso serve
      # tanto pra decidir alta cardinalidade quanto como missingness total,
      # evitando um job Spark separado pra missingness.
      card <- tryCatch(.agg_cardinality(.data, var), error = function(e) NULL)
      miss_total <- .card_to_miss_total(card)

      HIGH_CARD <- 10L
      is_high_card <- FALSE
      if (cls %in% c("character", "logical") && !is.null(card)) {
        if (card$n_distinct[1] >= HIGH_CARD) is_high_card <- TRUE
      }

      if (is_high_card) {
        hc <- list(card = card)
        summaries[[var]] <- list(agg = list(high_card = hc),
                                  miss_total = miss_total)
        metas[[var]] <- .build_var_meta_high_card(hc, miss_total,
                                                  is_longitudinal = FALSE)
        metas[[var]]$label <- lbl
        .mini_high_card_notice_titled(hc, var, lbl)
      } else {
        agg <- .agg_var_cross(.data, var, cls,
                              n_bins = n_bins, top_n_cat = top_n_cat)
        summaries[[var]] <- list(agg = agg, miss_total = miss_total)
        metas[[var]] <- .build_var_meta(cls, agg, miss_total,
                                        is_longitudinal = FALSE)
        metas[[var]]$label <- lbl
        .plot_var_mini_cross(agg, var, var_class = cls,
                             label = lbl, miss_total = miss_total)
      }
    }, error = function(e) {
      .empty_panel(paste0(var, ": ", conditionMessage(e)))
    })
    if (!is.null(mp)) mini_plots[[var]] <- mp
  }

  # Monta grid 3-colunas com patchwork
  grid_plot <- NULL
  if (length(mini_plots) > 0 && .has_patchwork()) {
    grid_plot <- patchwork::wrap_plots(mini_plots, ncol = 3)
  }

  list(plots = mini_plots, summaries = summaries, metas = metas,
       extras = list(grid = grid_plot, n_vars = length(mini_plots)))
}

# -----------------------------------------------------------------------------
# .inspect_variables_longitudinal()
# -----------------------------------------------------------------------------

.inspect_variables_longitudinal <- function(.data, variables = NULL,
                                             labels = list(),
                                             id_var, time_var = NULL,
                                             n_bins = 30,
                                             top_n_cat = 8,
                                             treat_as_categorical = NULL) {

  if (is.null(id_var) || !nzchar(id_var)) {
    stop("[autocodebook] id_var eh obrigatorio em inspecao longitudinal.",
         call. = FALSE)
  }
  if (is.null(time_var) || !nzchar(time_var)) {
    stop("[autocodebook] time_var eh obrigatorio em inspecao longitudinal.",
         call. = FALSE)
  }

  all_vars <- if (inherits(.data, "tbl_spark")) colnames(.data) else names(.data)

  if (is.null(variables)) {
    skip <- c(id_var, time_var)
    variables <- setdiff(all_vars, skip)
  } else {
    variables <- intersect(variables, all_vars)
  }

  plots <- list()
  summaries <- list()
  extras <- list()
  metas <- list()

  for (var in variables) {
    cls <- .infer_data_class(.data, var, id_var = id_var)
    # Override por argumento manual
    if (!is.null(treat_as_categorical) && var %in% treat_as_categorical) {
      cls <- "character"
    }
    if (cls == "id") next
    lbl <- if (!is.null(labels[[var]])) labels[[var]] else var

    plots[[var]] <- tryCatch({
      # CONSOLIDACAO: cardinalidade uma vez, deriva missingness total.
      card <- tryCatch(.agg_cardinality(.data, var), error = function(e) NULL)
      miss_total <- .card_to_miss_total(card)

      HIGH_CARD <- 10L
      is_high_card <- FALSE
      hc <- NULL
      if (cls %in% c("character", "logical") && !is.null(card)) {
        if (card$n_distinct[1] >= HIGH_CARD) {
          is_high_card <- TRUE
          hc <- list(card = card)
        }
      }

      if (is_high_card) {
        # Nao agrega by_time (distribuicao) — so missingness e intra-ID
        agg <- list(
          by_time      = NULL,
          miss_by_time = tryCatch(.agg_missingness_by_time(.data, var, time_var),
                                  error = function(e) NULL),
          intra        = tryCatch(.agg_intra_id_variation(.data, var, id_var),
                                  error = function(e) NULL)
        )
        summaries[[var]] <- list(agg = agg, miss_total = miss_total)
        metas[[var]] <- .build_var_meta_high_card(hc, miss_total,
                                                  is_longitudinal = TRUE,
                                                  agg = agg)
        metas[[var]]$label <- lbl
        .plot_var_panel_longitudinal(agg, var, var_class = cls,
                                      time_var = time_var, label = lbl,
                                      miss_total = miss_total,
                                      high_card = TRUE, hc = hc)
      } else {
        agg <- .agg_var_long(.data, var, cls, id_var, time_var,
                             top_n_cat = top_n_cat)
        summaries[[var]] <- list(agg = agg, miss_total = miss_total)
        metas[[var]] <- .build_var_meta(cls, agg, miss_total,
                                        is_longitudinal = TRUE)
        metas[[var]]$label <- lbl
        .plot_var_panel_longitudinal(agg, var, var_class = cls,
                                      time_var = time_var, label = lbl,
                                      miss_total = miss_total)
      }
    }, error = function(e) {
      .empty_panel(paste0(var, ": ", conditionMessage(e)))
    })
  }

  plots <- plots[!vapply(plots, is.null, logical(1))]
  list(plots = plots, summaries = summaries, metas = metas, extras = extras)
}

# -----------------------------------------------------------------------------
# .build_var_meta_high_card — meta para variavel de alta cardinalidade.
# Mantem a mesma estrutura de .build_var_meta para o template nao quebrar.
# -----------------------------------------------------------------------------

.build_var_meta_high_card <- function(hc, miss_total, is_longitudinal,
                                       agg = NULL) {
  meta <- list(
    class       = "character",
    pct_missing = NA_real_,
    badge_label = NULL,
    badge_kind  = "none",
    n_levels    = NA_integer_,
    is_high_card = TRUE
  )
  if (!is.null(hc) && !is.null(hc$card)) {
    meta$n_levels <- as.integer(hc$card$n_distinct[1])
  }
  if (!is.null(miss_total) && nrow(miss_total) == 1) {
    meta$pct_missing <- miss_total$pct_missing
  }
  # Badge de variacao intra-ID (longitudinal)
  if (is_longitudinal && !is.null(agg) && !is.null(agg$intra) &&
      nrow(agg$intra) > 0) {
    total    <- sum(agg$intra$n)
    n_fixa   <- sum(agg$intra$n[agg$intra$n_distinct == 1L])
    pct_fixa <- if (total > 0) n_fixa / total else NA_real_
    if (!is.na(pct_fixa)) {
      if (pct_fixa >= 0.5) {
        meta$badge_label <- sprintf("Fixa %s", .pct_str(pct_fixa))
        meta$badge_kind  <- "fixed"
      } else {
        meta$badge_label <- sprintf("Varia %s", .pct_str(1 - pct_fixa))
        meta$badge_kind  <- "varies"
      }
    }
  }
  if (!is.na(meta$pct_missing) && meta$pct_missing > 0.20) {
    meta$badge_label <- sprintf("%s missing", .pct_str(meta$pct_missing))
    meta$badge_kind  <- "missing_high"
  }
  meta
}

# -----------------------------------------------------------------------------
# .build_var_meta — extrai metadados de UMA variavel para o card do dashboard
# Retorna list(class, pct_missing, badge_label, badge_kind, n_levels)
# badge_kind in {"fixed","varies","missing_high","none"}
# -----------------------------------------------------------------------------

.build_var_meta <- function(var_class, agg, miss_total, is_longitudinal) {

  meta <- list(
    class       = var_class,
    pct_missing = NA_real_,
    badge_label = NULL,
    badge_kind  = "none",
    n_levels    = NA_integer_
  )

  # % missing
  if (!is.null(miss_total) && nrow(miss_total) == 1) {
    meta$pct_missing <- miss_total$pct_missing
  }

  # n_levels para categoricas
  if (var_class %in% c("character", "logical") && !is.null(agg$cat)) {
    meta$n_levels <- nrow(agg$cat)
  }

  # Badge de variacao intra-ID (so longitudinal, quando ha dados intra)
  if (is_longitudinal && !is.null(agg$intra) && nrow(agg$intra) > 0) {
    total     <- sum(agg$intra$n)
    n_fixa    <- sum(agg$intra$n[agg$intra$n_distinct == 1L])
    pct_fixa  <- if (total > 0) n_fixa / total else NA_real_
    if (!is.na(pct_fixa)) {
      if (pct_fixa >= 0.5) {
        meta$badge_label <- sprintf("Fixa %s",
                                    .pct_str(pct_fixa))
        meta$badge_kind  <- "fixed"
      } else {
        meta$badge_label <- sprintf("Varia %s",
                                    .pct_str(1 - pct_fixa))
        meta$badge_kind  <- "varies"
      }
    }
  }

  # Se missingness alta (>20%), badge de alerta tem prioridade
  if (!is.na(meta$pct_missing) && meta$pct_missing > 0.20) {
    meta$badge_label <- sprintf("%s missing", .pct_str(meta$pct_missing))
    meta$badge_kind  <- "missing_high"
  }

  meta
}

#' @keywords internal
.pct_str <- function(x) {
  if (is.na(x)) return("NA")
  paste0(formatC(100 * x, format = "f", digits = 1), "%")
}
