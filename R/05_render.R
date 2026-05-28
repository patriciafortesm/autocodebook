# =============================================================================
# autocodebook — Renderização e exportação
# =============================================================================

# =============================================================================

# =============================================================================
# cb_render() — Renderiza codebook como gt table
# =============================================================================

#' Render the codebook as a gt table
#'
#' @param group_by_block Logical. If TRUE and blocks are defined,
#'   groups rows by block. Default: TRUE.
#' @param show_code Logical. Show the "code" column? Default: TRUE.
#'
#' @return A gt object.
#' @export
#'
#' @examples
#' cb_init(id_col = "id")
#' cb_register("age", label = "Age in years", type = "integer")
#' cb_render()
cb_render <- function(group_by_block = TRUE, show_code = TRUE) {
  cb <- .cb_env$codebook

  if (nrow(cb) == 0) {
    message("[autocodebook] Codebook vazio - nenhuma variavel registrada.")
    return(invisible(NULL))
  }

  # Formata categorias e código para HTML
  cb <- cb %>%
    dplyr::mutate(
      categories = gsub("; ", "<br>", categories, fixed = TRUE),
      code = paste0(
        "<div style='white-space:pre-wrap;font-family:monospace;font-size:0.82em;'>",
        code, "</div>"
      )
    )

  # Remove coluna code se não desejada
  if (!show_code) cb <- cb %>% dplyr::select(-code)

  # Remove coluna block antes de montar gt (será usada para agrupamento se necessário)
  has_blocks <- group_by_block && "block" %in% names(cb) && any(cb$block != "")

  if (!has_blocks) {
    cb <- cb %>% dplyr::select(-dplyr::any_of("block"))
  }

  # Monta gt
  tbl <- cb %>%
    gt() %>%
    tab_header(
      title    = "Codebook autom\u00e1tico",
      subtitle = "Gerado por introspec\u00e7\u00e3o via autocodebook"
    )

  # Labels das colunas
  if (show_code) {
    tbl <- tbl %>%
      cols_label(
        variable   = "Vari\u00e1vel",
        type       = "Tipo",
        source     = "Origem",
        label      = "R\u00f3tulo",
        categories = "Categorias",
        code       = "C\u00f3digo de gera\u00e7\u00e3o"
      )
  } else {
    tbl <- tbl %>%
      cols_label(
        variable   = "Vari\u00e1vel",
        type       = "Tipo",
        source     = "Origem",
        label      = "R\u00f3tulo",
        categories = "Categorias"
      )
  }

  # Markdown rendering
  md_cols <- "categories"
  if (show_code) md_cols <- c(md_cols, "code")
  tbl <- tbl %>% fmt_markdown(columns = dplyr::all_of(md_cols))

  # Larguras
  if (show_code) {
    tbl <- tbl %>%
      cols_width(
        variable   ~ px(180),
        type       ~ px(80),
        source     ~ px(220),
        label      ~ px(200),
        categories ~ px(280),
        code       ~ px(520)
      )
  } else {
    tbl <- tbl %>%
      cols_width(
        variable   ~ px(180),
        type       ~ px(80),
        source     ~ px(220),
        label      ~ px(200),
        categories ~ px(280)
      )
  }

  # Agrupamento por bloco
  if (has_blocks) {
    tbl <- tbl %>%
      cols_label(block = "") %>%
      tab_row_group(
        label = "Sem bloco definido",
        rows  = block == ""
      )
    blocks_defined <- unique(cb$block[cb$block != ""])
    for (b in rev(blocks_defined)) {
      tbl <- tbl %>%
        tab_row_group(label = b, rows = block == b)
    }
  }

  # Negrito: titulo + cabecalho de coluna + nomes dos blocos (row groups).
  tbl <- tbl %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_title(groups = "title")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(dplyr::everything())
    ) %>%
    gt::tab_options(table.font.size = gt::px(14))
  if (has_blocks) {
    tbl <- tbl %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_row_groups()
      )
  }

  tbl
}

# =============================================================================
# track_render() — Renderiza tracking como gt table
# =============================================================================

#' Render the tracking log as a gt table
#'
#' @param show_elapsed Logical. Show the elapsed_s column if present?
#'   Default: FALSE.
#'
#' @return A gt object.
#' @export
#'
#' @examples
#' cb_init(id_col = "id")
#' df <- tibble::tibble(id = 1:50)
#' track_step(df, "Initial cohort")
#' track_render()
track_render <- function(show_elapsed = FALSE) {
  tr <- .cb_env$tracking

  if (nrow(tr) == 0) {
    message("[autocodebook] Tracking vazio \u2014 nenhuma etapa registrada.")
    return(invisible(NULL))
  }

  if (!isTRUE(show_elapsed) && "elapsed_s" %in% names(tr)) {
    tr <- tr %>% dplyr::select(-elapsed_s)
  }

  tbl <- tr %>%
    gt() %>%
    tab_header(
      title    = "Tracking table \u2014 fluxograma de elegibilidade",
      subtitle = "N de indiv\u00edduos \u00fanicos em cada etapa"
    )

  if (isTRUE(show_elapsed) && "elapsed_s" %in% names(tr)) {
    tbl <- tbl %>%
      cols_label(
        step        = "Etapa",
        description = "Descri\u00e7\u00e3o",
        n_ids       = "N (indiv\u00edduos)",
        n_removed   = "Removidos",
        elapsed_s   = "Tempo (s)"
      ) %>%
      fmt_number(columns = c(n_ids, n_removed), decimals = 0) %>%
      fmt_number(columns = elapsed_s, decimals = 2)
  } else {
    tbl <- tbl %>%
      cols_label(
        step        = "Etapa",
        description = "Descri\u00e7\u00e3o",
        n_ids       = "N (indiv\u00edduos)",
        n_removed   = "Removidos"
      ) %>%
      fmt_number(columns = c(n_ids, n_removed), decimals = 0)
  }

  tbl %>%
    tab_style(
      style     = cell_fill(color = "#FFF3CD"),
      locations = cells_body(columns = n_removed, rows = n_removed > 0)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_title(groups = "title")
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels(dplyr::everything())
    ) %>%
    gt::tab_options(table.font.size = gt::px(14))
}

# =============================================================================
# cb_export() / track_export() — Salva em HTML ou CSV
# =============================================================================

#' Export codebook to file
#'
#' Supports multiple formats based on file extension:
#'   - `.html` - rendered gt table (presentation)
#'   - `.csv`  - raw tibble (programmatic reuse)
#'   - `.docx` - editable Word table (paper supplements, presentations)
#'   - `.xlsx` - editable spreadsheet with filters
#'
#' For `.docx` and `.xlsx`, you can pass `variables = c(...)` to export only
#' a subset (useful for paper supplements / presentations).
#'
#' @param path File path. Extension determines format.
#' @param variables Optional character vector. If provided, exports only
#'   these variables (in the given order). Default: all.
#' @param ... Additional arguments passed to cb_render() for HTML (e.g.
#'   show_code).
#'
#' @return Invisible path.
#' @export
#'
#' @examples
#' cb_init(id_col = "id")
#' cb_register("age", label = "Age in years", type = "integer")
#' out <- file.path(tempdir(), "codebook.csv")
#' cb_export(out)
cb_export <- function(path = "codebook.html", variables = NULL, ...) {
  ext <- tolower(tools::file_ext(path))

  cb <- .cb_env$codebook
  if (!is.null(variables)) {
    missing_vars <- setdiff(variables, cb$variable)
    if (length(missing_vars) > 0) {
      warning("[autocodebook] Variaveis nao encontradas no codebook: ",
              paste(missing_vars, collapse = ", "))
    }
    cb <- cb[match(intersect(variables, cb$variable), cb$variable), , drop = FALSE]
  }

  if (ext == "csv") {
    utils::write.csv(cb, path, row.names = FALSE)
  } else if (ext == "docx") {
    .cb_export_docx(cb, path)
  } else if (ext == "xlsx") {
    .cb_export_xlsx(cb, path)
  } else {
    # HTML default: usa cb_render (que opera sobre o codebook completo,
    # entao filtramos antes restaurando temporariamente)
    if (!is.null(variables)) {
      old <- .cb_env$codebook
      on.exit(.cb_env$codebook <- old, add = TRUE)
      .cb_env$codebook <- cb
    }
    tbl <- cb_render(...)
    if (!is.null(tbl)) gtsave(tbl, path)
  }
  message("[autocodebook] Codebook salvo em: ", path)
  invisible(path)
}

#' Export tracking table to file
#'
#' Supports `.html`, `.csv`, `.docx`, and `.xlsx`.
#'
#' @param path File path.
#' @param show_elapsed Logical. Include elapsed_s column? Default: FALSE.
#' @return Invisible path.
#' @export
#'
#' @examples
#' cb_init(id_col = "id")
#' df <- tibble::tibble(id = 1:50)
#' track_step(df, "Initial cohort")
#' out <- file.path(tempdir(), "tracking_table.csv")
#' track_export(out)
track_export <- function(path = "tracking_table.html", show_elapsed = FALSE) {
  ext <- tolower(tools::file_ext(path))
  tr <- .cb_env$tracking
  if (!isTRUE(show_elapsed) && "elapsed_s" %in% names(tr)) {
    tr <- tr %>% dplyr::select(-elapsed_s)
  }

  if (ext == "csv") {
    utils::write.csv(tr, path, row.names = FALSE)
  } else if (ext == "docx") {
    .track_export_docx(tr, path)
  } else if (ext == "xlsx") {
    .track_export_xlsx(tr, path)
  } else {
    tbl <- track_render(show_elapsed = show_elapsed)
    if (!is.null(tbl)) gtsave(tbl, path)
  }
  message("[autocodebook] Tracking salvo em: ", path)
  invisible(path)
}

# =============================================================================
# Helpers de export — DOCX e XLSX
# =============================================================================
# Dependencias suaves: officer + flextable (docx), openxlsx (xlsx).
# Se nao instaladas, da mensagem clara.

.require_pkg <- function(pkg, purpose) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("[autocodebook] Pacote '", pkg, "' necessario para ", purpose,
         ". Instale com: install.packages('", pkg, "')",
         call. = FALSE)
  }
}

.cb_export_docx <- function(cb, path) {
  .require_pkg("officer",   "export DOCX")
  .require_pkg("flextable", "export DOCX")

  ft <- flextable::flextable(cb) %>%
    flextable::set_header_labels(
      variable   = "Variavel",
      type       = "Tipo",
      source     = "Origem",
      label      = "Rotulo",
      categories = "Categorias",
      code       = "Codigo de geracao",
      block      = "Bloco"
    ) %>%
    flextable::autofit() %>%
    flextable::theme_booktabs()

  doc <- officer::read_docx() %>%
    officer::body_add_par("Codebook automatico", style = "heading 1") %>%
    flextable::body_add_flextable(ft)

  print(doc, target = path)
  invisible(path)
}

.cb_export_xlsx <- function(cb, path) {
  .require_pkg("openxlsx", "export XLSX")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "codebook")
  openxlsx::writeDataTable(wb, "codebook", cb,
                           tableStyle = "TableStyleMedium2",
                           withFilter = TRUE)
  openxlsx::setColWidths(wb, "codebook",
                         cols = seq_len(ncol(cb)),
                         widths = "auto")
  openxlsx::freezePane(wb, "codebook", firstRow = TRUE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

.track_export_docx <- function(tr, path) {
  .require_pkg("officer",   "export DOCX")
  .require_pkg("flextable", "export DOCX")

  labels <- list(
    step = "Etapa", description = "Descricao",
    n_ids = "N (individuos)", n_removed = "Removidos",
    elapsed_s = "Tempo (s)"
  )
  ft <- flextable::flextable(tr) %>%
    flextable::set_header_labels(values = labels[names(tr)]) %>%
    flextable::autofit() %>%
    flextable::theme_booktabs()

  doc <- officer::read_docx() %>%
    officer::body_add_par("Tracking table \u2014 fluxograma de elegibilidade",
                          style = "heading 1") %>%
    flextable::body_add_flextable(ft)

  print(doc, target = path)
  invisible(path)
}

.track_export_xlsx <- function(tr, path) {
  .require_pkg("openxlsx", "export XLSX")

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "tracking")
  openxlsx::writeDataTable(wb, "tracking", tr,
                           tableStyle = "TableStyleMedium2",
                           withFilter = TRUE)
  openxlsx::setColWidths(wb, "tracking",
                         cols = seq_len(ncol(tr)), widths = "auto")
  openxlsx::freezePane(wb, "tracking", firstRow = TRUE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
