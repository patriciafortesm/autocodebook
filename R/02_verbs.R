# =============================================================================
# autocodebook — Wrappers de verbos dplyr/sparklyr
# =============================================================================
# auto_mutate()    -> mutate + registro automatico no codebook
# auto_summarise() -> summarise + registro automatico
# auto_filter()    -> filter + registro automatico no tracking
# =============================================================================


# =============================================================================
# auto_mutate()
# =============================================================================

#' Mutate with automatic codebook registration
#'
#' Works exactly like [dplyr::mutate()], but also captures each expression
#' and registers the resulting variable in the codebook. Type, source columns,
#' categories, and source code are inferred automatically - you only need to
#' provide human-readable labels.
#'
#' @param .data A Spark DataFrame (tbl_spark) or local data frame.
#' @param labels Named list mapping variable names to labels (descriptions).
#'   Variables not in this list get their own name as label.
#' @param block Optional character label for the pipeline block/section
#'   (e.g. "Demographic variables"). Groups variables in the codebook.
#' @param ... Named expressions, same syntax as `dplyr::mutate()`.
#'
#' @return The transformed data frame (same class as input).
#' @export
auto_mutate <- function(.data, labels = list(), block = "", ...) {
  dots <- rlang::enquos(...)

  existing_cols <- if (inherits(.data, "tbl_spark")) {
    colnames(.data)
  } else {
    names(.data)
  }

  result <- mutate(.data, !!!dots)

  for (var_name in names(dots)) {
    code_text <- rlang::quo_text(dots[[var_name]])
    code_text_clean <- gsub("\\s+", " ", code_text)

    entry <- tibble(
      variable   = var_name,
      type       = .infer_type(code_text_clean),
      source     = .infer_source(code_text_clean, existing_cols),
      label      = if (!is.null(labels[[var_name]])) labels[[var_name]] else var_name,
      categories = .infer_categories(code_text_clean),
      code       = code_text_clean,
      block      = block
    )

    .cb_env$codebook <- .cb_env$codebook %>%
      filter(variable != var_name) %>%
      bind_rows(entry)
  }

  result
}

# =============================================================================
# auto_summarise()
# =============================================================================

#' Summarise with automatic codebook registration
#'
#' Works exactly like [dplyr::summarise()], but also captures each expression
#' and registers the resulting variable in the codebook.
#'
#' @inheritParams auto_mutate
#' @param .groups Grouping behavior after summarise. Default: "drop".
#'
#' @return The summarised data frame.
#' @export
auto_summarise <- function(.data, labels = list(), block = "", ...,
                           .groups = "drop") {
  dots <- rlang::enquos(...)

  existing_cols <- if (inherits(.data, "tbl_spark")) {
    colnames(.data)
  } else {
    names(.data)
  }

  result <- summarise(.data, !!!dots, .groups = .groups)

  for (var_name in names(dots)) {
    code_text <- rlang::quo_text(dots[[var_name]])
    code_text_clean <- gsub("\\s+", " ", code_text)

    entry <- tibble(
      variable   = var_name,
      type       = .infer_type(code_text_clean),
      source     = .infer_source(code_text_clean, existing_cols),
      label      = if (!is.null(labels[[var_name]])) labels[[var_name]] else var_name,
      categories = .infer_categories(code_text_clean),
      code       = code_text_clean,
      block      = block
    )

    .cb_env$codebook <- .cb_env$codebook %>%
      filter(variable != var_name) %>%
      bind_rows(entry)
  }

  result
}

# =============================================================================
# auto_filter()
# =============================================================================

#' Filter with automatic tracking
#'
#' Works exactly like [dplyr::filter()], but also logs a tracking step
#' recording how many unique IDs remain after the filter.
#'
#' The signature mirrors v0.1.0 for full backward compatibility: `step`
#' and `description` come first (so existing positional calls keep working),
#' then `...` for the filter conditions, and finally the new big-data
#' options (`cache`, `assume_unique`) which **must be passed by name**.
#'
#' @param .data A Spark DataFrame or local data frame.
#' @param step Character label for this filtering step.
#' @param description Character description of the filter.
#' @param ... Filter conditions, same syntax as `dplyr::filter()`.
#' @param cache Logical or NULL (named-only). If TRUE, materializes the
#'   result with `cb_checkpoint()` after filtering - useful in long Spark
#'   pipelines. If NULL, falls back to the session default (set via
#'   `cb_init()` or `cb_set_default_cache()`). Default: NULL.
#' @param assume_unique Logical (named-only). Passed to `track_step()`.
#'   Set TRUE only when you are certain the ID column has no duplicates
#'   at this stage. Default: FALSE.
#'
#' @return The filtered data frame.
#' @export
auto_filter <- function(.data, step = "", description = "", ...,
                        cache = NULL, assume_unique = FALSE) {
  result <- dplyr::filter(.data, ...)

  use_cache <- if (is.null(cache)) isTRUE(.cb_env$default_cache) else isTRUE(cache)
  if (use_cache && inherits(result, "tbl_spark")) {
    result <- cb_checkpoint(result, mode = "memory")
  }

  track_step(result, step, description, assume_unique = assume_unique)
  result
}
