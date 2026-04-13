# =============================================================================
# autocodebook — Renderização e exportação
# =============================================================================

#' @importFrom gt gt tab_header cols_label fmt_number fmt_markdown cols_width
#' @importFrom gt tab_style cell_fill cells_body px gtsave tab_row_group

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
cb_render <- function(group_by_block = TRUE, show_code = TRUE) {
  cb <- .cb_env$codebook

  if (nrow(cb) == 0) {
    message("[autocodebook] Codebook vazio — nenhuma variável registrada.")
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

  tbl
}

# =============================================================================
# track_render() — Renderiza tracking como gt table
# =============================================================================

#' Render the tracking log as a gt table
#' @return A gt object.
#' @export
track_render <- function() {
  tr <- .cb_env$tracking

  if (nrow(tr) == 0) {
    message("[autocodebook] Tracking vazio — nenhuma etapa registrada.")
    return(invisible(NULL))
  }

  tr %>%
    gt() %>%
    tab_header(
      title    = "Tracking table \u2014 fluxograma de elegibilidade",
      subtitle = "N de indiv\u00edduos \u00fanicos em cada etapa"
    ) %>%
    cols_label(
      step        = "Etapa",
      description = "Descri\u00e7\u00e3o",
      n_ids       = "N (indiv\u00edduos)",
      n_removed   = "Removidos"
    ) %>%
    fmt_number(columns = c(n_ids, n_removed), decimals = 0) %>%
    tab_style(
      style     = cell_fill(color = "#FFF3CD"),
      locations = cells_body(columns = n_removed, rows = n_removed > 0)
    )
}

# =============================================================================
# cb_export() / track_export() — Salva em HTML ou CSV
# =============================================================================

#' Export codebook to file
#'
#' @param path File path. Extension determines format:
#'   `.html` uses gtsave, `.csv` uses write.csv.
#' @param ... Additional arguments passed to cb_render() (e.g. show_code).
#'
#' @return Invisible path.
#' @export
cb_export <- function(path = "codebook.html", ...) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    utils::write.csv(.cb_env$codebook, path, row.names = FALSE)
  } else {
    tbl <- cb_render(...)
    if (!is.null(tbl)) gtsave(tbl, path)
  }
  message("[autocodebook] Codebook salvo em: ", path)
  invisible(path)
}

#' Export tracking table to file
#'
#' @param path File path (.html or .csv).
#' @return Invisible path.
#' @export
track_export <- function(path = "tracking_table.html") {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    utils::write.csv(.cb_env$tracking, path, row.names = FALSE)
  } else {
    tbl <- track_render()
    if (!is.null(tbl)) gtsave(tbl, path)
  }
  message("[autocodebook] Tracking salvo em: ", path)
  invisible(path)
}
