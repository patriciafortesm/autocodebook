# =============================================================================
# autocodebook — Registro manual e acessores
# =============================================================================

# =============================================================================
# cb_register() — Registro manual (fallback)
# =============================================================================

#' Manually register a variable in the codebook
#'
#' Use when auto_mutate/auto_summarise doesn't apply — e.g. variables
#' created via window_order + row_number in a separate pipeline step.
#'
#' @param var Variable name (character).
#' @param label Human-readable description.
#' @param type Optional. If NULL, defaults to "character".
#' @param source Optional. Source column(s).
#' @param categories Optional. Category descriptions.
#' @param code Optional. Code that generated the variable.
#' @param block Optional. Pipeline block label.
#'
#' @return Invisible NULL.
#' @export
cb_register <- function(var, label, type = NULL, source = "",
                        categories = "", code = "", block = "") {
  if (is.null(type)) type <- "character"

  entry <- tibble(
    variable   = var,
    type       = type,
    source     = source,
    label      = label,
    categories = categories,
    code       = code,
    block      = block
  )

  .cb_env$codebook <- .cb_env$codebook %>%
    filter(variable != var) %>%
    bind_rows(entry)

  invisible(NULL)
}

# =============================================================================
# cb_get() / cb_reset()
# =============================================================================

#' Get the current codebook as a tibble
#' @return A tibble with all registered variables.
#' @export
cb_get <- function() {
  .cb_env$codebook
}

#' Reset the codebook (clear all entries)
#' @return Invisible NULL.
#' @export
cb_reset <- function() {
  .cb_env$codebook <- tibble(
    variable = character(), type = character(), source = character(),
    label = character(), categories = character(), code = character(),
    block = character()
  )
  invisible(NULL)
}
