# =============================================================================
# autocodebook — Wrappers de verbos dplyr/sparklyr
# =============================================================================
# auto_mutate()    → mutate + registro automático no codebook
# auto_summarise() → summarise + registro automático
# auto_filter()    → filter + registro automático no tracking
# =============================================================================

#' @import rlang
#' @importFrom dplyr mutate summarise filter select distinct

# =============================================================================
# auto_mutate()
# =============================================================================

#' Mutate with automatic codebook registration
#'
#' Works exactly like [dplyr::mutate()], but also captures each expression
#' and registers the resulting variable in the codebook. Type, source columns,
#' categories, and source code are inferred automatically — you only need to
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
#'
#' @examples
#' \dontrun{
#' df <- auto_mutate(df,
#'   labels = list(sex = "Sex", age_cat = "Age category"),
#'   block  = "Demographics",
#'   sex = case_when(
#'     cod_sexo == 1L ~ "Male",
#'     cod_sexo == 2L ~ "Female",
#'     TRUE           ~ NA_character_
#'   ),
#'   age_cat = case_when(
#'     age < 18  ~ "Child",
#'     age < 65  ~ "Adult",
#'     TRUE      ~ "Elderly"
#'   )
#' )
#' }
auto_mutate <- function(.data, labels = list(), block = "", ...) {
  dots <- rlang::enquos(...)

  # Colunas existentes ANTES do mutate
  existing_cols <- if (inherits(.data, "tbl_spark")) {
    colnames(.data)
  } else {
    names(.data)
  }

  # Executa o mutate normalmente
  result <- mutate(.data, !!!dots)

  # Registra cada variável no codebook
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
#'
#' @examples
#' \dontrun{
#' summary <- df %>%
#'   group_by(id) %>%
#'   auto_summarise(
#'     labels = list(total = "Total count", first_date = "First observed date"),
#'     block  = "Migration summary",
#'     total      = n(),
#'     first_date = min(date, na.rm = TRUE),
#'     .groups = "drop"
#'   )
#' }
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
#' @param .data A Spark DataFrame or local data frame.
#' @param step Character label for this filtering step.
#' @param description Character description of the filter.
#' @param ... Filter conditions, same syntax as `dplyr::filter()`.
#'
#' @return The filtered data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- auto_filter(df,
#'   step = "3. Remove missing dates",
#'   description = "Exclui registros sem data de referência",
#'   !is.na(dt_reference)
#' )
#' }
auto_filter <- function(.data, step = "", description = "", ...) {
  result <- filter(.data, ...)
  track_step(result, step, description)
  result
}
