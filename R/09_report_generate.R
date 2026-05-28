# =============================================================================
# autocodebook — generate_report() : entrypoint do modulo de relatorio
# =============================================================================
# Gera um relatorio HTML autocontido com:
#   1. Resumo do dataset
#   2. Fluxograma de elegibilidade (lendo .cb_env$tracking)
#   3. Codebook (lendo .cb_env$codebook)
#   4. Inspecao de variaveis (transversal ou longitudinal)
# =============================================================================

#' Generate a standardized report from the current session
#'
#' Produces an HTML report combining the eligibility flowchart, the codebook,
#' and a per-variable inspection panel. Supports two inspection modes:
#'
#' - `cross_sectional`: one plot per variable (histogram / bar / time).
#' - `longitudinal`: three plots per variable (global distribution, intra-ID
#'   variation, missingness by time) plus a meta plot of observations per ID.
#'
#' All aggregations happen in Spark/dplyr; only small summaries are collected.
#'
#' @param data A Spark DataFrame (tbl_spark) or local data frame.
#' @param type One of `"cross_sectional"` or `"longitudinal"`.
#' @param id_var Character. Name of the ID column. For `longitudinal`,
#'   mandatory. For `cross_sectional`, used to skip the ID column in inspection.
#' @param time_var Character or NULL. Name of the time/wave column.
#'   Used in `longitudinal` to compute missingness-over-time. Default: NULL.
#' @param variables Optional character vector. If provided, inspects only
#'   these variables. Default: NULL (all except id_var/time_var).
#' @param labels Optional named list (variable -> label). If NULL, uses
#'   labels from the codebook when available.
#' @param treat_as_categorical Character vector of variable names to treat
#'   as categorical even when their R class is numeric or integer. Useful
#'   for coded variables (e.g. `cod_sexo` stored as 1L/2L, `cod_raca`
#'   stored as integer). For these variables, the report uses bar charts
#'   and proportion-by-time stacked plots instead of histograms / median+IQR.
#'   Default: NULL.
#' @param output_html File path for the HTML output. Default: "report.html".
#' @param output_dir Optional directory for ancillary files (codebook.xlsx,
#'   codebook.docx, etc.). If NULL, derived from output_html.
#' @param export_codebook_editable Logical. Also export codebook as
#'   .docx and .xlsx in `output_dir`. Default: TRUE.
#' @param cache_data Logical. If TRUE and `data` is a tbl_spark, persists
#'   the dataset once before the report aggregations, then releases it on
#'   exit. No-op for local data frames. Default: TRUE.
#' @param title Optional title for the report.
#' @param n_bins Number of bins for numeric histograms. Default: 30.
#' @param top_n_cat Max categories shown in categorical plots. Default: 20.
#'
#' @return Invisible list with paths to all generated files.
#' @export
#'
#' @examples
#' \dontrun{
#' # Transversal
#' generate_report(df_baseline, type = "cross_sectional",
#'                 id_var = "id_indiv",
#'                 treat_as_categorical = c("cod_sexo", "cod_raca"),
#'                 output_html = "report_baseline.html")
#'
#' # Longitudinal
#' generate_report(df_long, type = "longitudinal",
#'                 id_var   = "id_indiv",
#'                 time_var = "ano_referencia",
#'                 treat_as_categorical = c("cod_sexo"),
#'                 output_html = "report_longitudinal.html")
#' }
generate_report <- function(data,
                            type = c("cross_sectional", "longitudinal"),
                            id_var = NULL,
                            time_var = NULL,
                            variables = NULL,
                            labels = NULL,
                            treat_as_categorical = NULL,
                            output_html = "autocodebook_report.html",
                            output_dir = NULL,
                            export_codebook_editable = TRUE,
                            cache_data = TRUE,
                            title = NULL,
                            n_bins = 30,
                            top_n_cat = 20) {

  type <- match.arg(type)

  # Deps obrigatorias para o relatorio HTML
  .require_pkg("rmarkdown", "gerar o relatorio HTML")
  .require_pkg("knitr",     "gerar o relatorio HTML")
  if (!.has_ggplot2()) {
    stop("[autocodebook] Pacote 'ggplot2' necessario. ",
         "Instale com: install.packages('ggplot2')",
         call. = FALSE)
  }
  if (!.has_patchwork()) {
    stop("[autocodebook] Pacote 'patchwork' necessario para os paineis ",
         "compostos do relatorio. ",
         "Instale com: install.packages('patchwork')",
         call. = FALSE)
  }
  if (!.has_scales()) {
    stop("[autocodebook] Pacote 'scales' necessario para formatacao ",
         "dos eixos. Instale com: install.packages('scales')",
         call. = FALSE)
  }

  # Resolve labels do codebook se nao fornecidos
  if (is.null(labels)) {
    cb <- .cb_env$codebook
    if (nrow(cb) > 0) {
      labels <- as.list(stats::setNames(cb$label, cb$variable))
    } else {
      labels <- list()
    }
  }

  # Resolve output_dir
  if (is.null(output_dir)) {
    output_dir <- dirname(normalizePath(output_html, mustWork = FALSE))
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  ttl <- if (!is.null(title)) title else "Relatorio autocodebook"

  # =========================================================================
  # 0. Cache do dataset (CRITICO em big data)
  # =========================================================================
  # O relatorio faz dezenas de agregacoes sobre `data`. Se for um tbl_spark
  # vindo de uma cadeia lazy, cada agregacao re-executa o pipeline inteiro.
  # Persistir UMA vez garante que as agregacoes leem de memoria/disco.
  # Liberado (unpersist) no fim via on.exit.
  cached_here <- FALSE
  if (isTRUE(cache_data) && inherits(data, "tbl_spark") &&
      requireNamespace("sparklyr", quietly = TRUE)) {
    .cb_msg("[autocodebook] Persistindo dataset (sdf_persist MEMORY_AND_DISK)...")
    data <- tryCatch({
      d <- sparklyr::sdf_persist(data, storage.level = "MEMORY_AND_DISK")
      # Forca materializacao com um count (dispara o cache de fato)
      invisible(sparklyr::sdf_nrow(d))
      cached_here <- TRUE
      d
    }, error = function(e) {
      .cb_msg("[autocodebook] Aviso: nao foi possivel persistir (",
              conditionMessage(e), "). Seguindo sem cache.")
      data
    })
    if (cached_here) {
      on.exit({
        tryCatch({
          if (exists("sdf_unpersist", where = asNamespace("sparklyr"),
                     inherits = FALSE)) {
            unpersist_fn <- get("sdf_unpersist", envir = asNamespace("sparklyr"))
            unpersist_fn(data)
          }
        }, error = function(e) NULL)
      }, add = TRUE)
    }
  }

  # =========================================================================
  # 1. Coleta sumarios do dataset (n, ncol, etc.)
  # =========================================================================
  .cb_msg("[autocodebook] Coletando sumario do dataset...")

  dataset_summary <- .summarize_dataset(data, id_var = id_var,
                                         time_var = time_var)

  # =========================================================================
  # 2. Tabelas de fluxo: tracking linear + arvore (se houver split)
  # =========================================================================
  .cb_msg("[autocodebook] Montando tabelas de elegibilidade...")
  flow_tree_table <- tryCatch(flow_table(), error = function(e) NULL)
  has_tree <- !is.null(flow_tree_table) && nrow(flow_tree_table) > 0

  # =========================================================================
  # 3. Inspecao de variaveis
  # =========================================================================
  .cb_msg("[autocodebook] Inspecionando variaveis (", type, ")...")

  # Se o usuario nao especificou `variables`, usa as variaveis registradas no
  # codebook (= o dataset final do pipeline). So inclui as que existem no
  # dataframe. Se o codebook estiver vazio, cai no comportamento antigo
  # (todas as colunas, menos id/time).
  if (is.null(variables) && nrow(.cb_env$codebook) > 0) {
    cols_data <- if (inherits(data, "tbl_spark")) colnames(data) else names(data)
    cb_vars   <- intersect(.cb_env$codebook$variable, cols_data)
    if (length(cb_vars) > 0) {
      variables <- cb_vars
      .cb_msg("[autocodebook] Inspecionando ", length(variables),
              " variaveis registradas no codebook.")
    }
  }

  inspection <- if (type == "cross_sectional") {
    .inspect_variables_cross_sectional(
      data, variables = variables, labels = labels,
      id_var = id_var, n_bins = n_bins, top_n_cat = top_n_cat,
      treat_as_categorical = treat_as_categorical
    )
  } else {
    .inspect_variables_longitudinal(
      data, variables = variables, labels = labels,
      id_var = id_var, time_var = time_var,
      n_bins = n_bins, top_n_cat = top_n_cat,
      treat_as_categorical = treat_as_categorical
    )
  }

  # =========================================================================
  # 4. Exports auxiliares (codebook editavel + tracking)
  # =========================================================================
  ancillary <- list()
  if (isTRUE(export_codebook_editable) && nrow(.cb_env$codebook) > 0) {
    .cb_msg("[autocodebook] Exportando codebook editavel...")
    try({
      p_docx <- file.path(output_dir, "codebook.docx")
      cb_export(p_docx)
      ancillary$codebook_docx <- p_docx
    }, silent = TRUE)
    try({
      p_xlsx <- file.path(output_dir, "codebook.xlsx")
      cb_export(p_xlsx)
      ancillary$codebook_xlsx <- p_xlsx
    }, silent = TRUE)
  }

  # Tabelas de elegibilidade editaveis (XLSX) — pra copiar/colar resultados
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    # Tracking linear (sempre que houver etapas)
    if (nrow(.cb_env$tracking) > 0) {
      try({
        tr <- .cb_env$tracking
        if ("elapsed_s" %in% names(tr)) tr$elapsed_s <- NULL
        p_track_xlsx <- file.path(output_dir, "tabela_elegibilidade.xlsx")
        openxlsx::write.xlsx(tr, p_track_xlsx, overwrite = TRUE)
        ancillary$tabela_elegibilidade_xlsx <- p_track_xlsx
      }, silent = TRUE)
    }
    # Tabela da arvore (so se houver split)
    if (has_tree) {
      try({
        p_tree_xlsx <- file.path(output_dir, "tabela_grupos.xlsx")
        openxlsx::write.xlsx(flow_tree_table, p_tree_xlsx, overwrite = TRUE)
        ancillary$tabela_grupos_xlsx <- p_tree_xlsx
      }, silent = TRUE)
    }
  }

  # =========================================================================
  # 5. Renderiza o RMarkdown
  # =========================================================================
  .cb_msg("[autocodebook] Renderizando HTML...")

  rmd_path <- system.file("rmarkdown", "report-template.Rmd",
                          package = "autocodebook")
  if (!nzchar(rmd_path) || !file.exists(rmd_path)) {
    # Fallback: usa template embarcado em string
    rmd_path <- .write_default_template(tempfile(fileext = ".Rmd"))
  }

  # Empacota tudo no env do render
  render_env <- new.env(parent = globalenv())
  render_env$report_title       <- ttl
  render_env$report_type        <- type
  render_env$report_id_var      <- id_var
  render_env$report_time_var    <- time_var
  render_env$report_dataset     <- dataset_summary
  render_env$report_codebook    <- .cb_env$codebook
  render_env$report_tracking    <- .cb_env$tracking
  render_env$report_flow_tree   <- flow_tree_table
  render_env$report_has_tree    <- has_tree
  render_env$report_inspection  <- inspection
  render_env$report_ancillary   <- ancillary

  rmarkdown::render(
    input       = rmd_path,
    output_file = basename(output_html),
    output_dir  = dirname(normalizePath(output_html, mustWork = FALSE)),
    envir       = render_env,
    quiet       = !isTRUE(.cb_env$verbose)
  )

  out <- c(list(report_html = output_html), ancillary)

  message("[autocodebook] Relatorio gerado: ", output_html)
  if (length(ancillary) > 0) {
    for (nm in names(ancillary)) {
      message("[autocodebook]   + ", ancillary[[nm]])
    }
  }

  invisible(out)
}

# =============================================================================
# Helpers internos
# =============================================================================

#' @keywords internal
#' @noRd
.summarize_dataset <- function(data, id_var = NULL, time_var = NULL) {

  n_rows <- if (inherits(data, "tbl_spark")) {
    as.integer(sparklyr::sdf_nrow(data))
  } else {
    nrow(data)
  }

  cols <- if (inherits(data, "tbl_spark")) colnames(data) else names(data)
  n_cols <- length(cols)

  n_ids <- NA_integer_
  if (!is.null(id_var) && id_var %in% cols) {
    n_ids <- tryCatch({
      if (inherits(data, "tbl_spark")) {
        data %>% dplyr::select(dplyr::all_of(id_var)) %>%
          dplyr::distinct() %>% sparklyr::sdf_nrow() %>% as.integer()
      } else {
        length(unique(data[[id_var]]))
      }
    }, error = function(e) NA_integer_)
  }

  time_range <- NULL
  if (!is.null(time_var) && time_var %in% cols) {
    time_range <- tryCatch({
      tv <- rlang::sym(time_var)
      data %>%
        dplyr::filter(!is.na(!!tv)) %>%
        dplyr::summarise(min_t = min(!!tv, na.rm = TRUE),
                         max_t = max(!!tv, na.rm = TRUE)) %>%
        dplyr::collect()
    }, error = function(e) NULL)
  }

  list(
    n_rows     = n_rows,
    n_cols     = n_cols,
    n_ids      = n_ids,
    cols       = cols,
    time_range = time_range
  )
}


#' @keywords internal
#' @noRd
.write_default_template <- function(path) {
  template <- '---
title: "`r report_title`"
output:
  html_document:
    toc: true
    toc_float: true
    theme: cosmo
    self_contained: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, dpi = 100)
.fig_w_panel <- if (report_type == "longitudinal") 11   else 8
.fig_h_panel <- if (report_type == "longitudinal") 3.4  else 2.8
```

# Resumo do dataset

```{r summary, results="asis"}
ds <- report_dataset
cat("- **Linhas:** ", format(ds$n_rows, big.mark = ","), "\\n")
cat("- **Colunas:** ", ds$n_cols, "\\n")
if (!is.na(ds$n_ids)) cat("- **Individuos unicos:** ", format(ds$n_ids, big.mark = ","), "\\n")
if (!is.null(ds$time_range)) cat("- **Periodo:** ", as.character(ds$time_range$min_t), " a ", as.character(ds$time_range$max_t), "\\n")
```

# Elegibilidade

```{r tracking-table}
if (nrow(report_tracking) > 0) {
  tr <- report_tracking
  if ("elapsed_s" %in% names(tr)) tr$elapsed_s <- NULL
  knitr::kable(tr, format.args = list(big.mark = "."))
}
```

```{r tree-table, eval=isTRUE(report_has_tree)}
if (isTRUE(report_has_tree)) knitr::kable(report_flow_tree)
```

# Codebook

```{r codebook}
if (nrow(report_codebook) > 0) cb_render(group_by_block = TRUE, show_code = TRUE) else cat("_Codebook vazio._")
```

# Inspecao de variaveis

```{r inspection-loop, results="asis"}
plots <- report_inspection$plots
src <- c()
for (var in names(plots)) {
  chunk_id <- paste0("var_", gsub("[^A-Za-z0-9_]", "_", var))
  src <- c(src,
    sprintf("## %s\\n", var),
    sprintf("```{r %s, fig.width=%s, fig.height=%s, echo=FALSE}", chunk_id, .fig_w_panel, .fig_h_panel),
    sprintf("print(report_inspection$plots[[%s]])", deparse(var)),
    "```\\n")
}
out <- knitr::knit_child(text = paste(src, collapse = "\\n"), quiet = TRUE, envir = environment())
cat(out)
```

---

*Gerado por `autocodebook::generate_report()` (template basico)*
'
  writeLines(template, path)
  path
}
