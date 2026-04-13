# =============================================================================
# autocodebook — Tracking de elegibilidade
# =============================================================================

#' Record a tracking step
#'
#' Counts unique individuals in the current data and logs the step.
#' Works with both tbl_spark and local data frames.
#'
#' @param sdf A Spark DataFrame or local data frame.
#' @param step_label Short label for the step.
#' @param description Longer description.
#'
#' @return Invisible integer: number of unique IDs.
#' @export
track_step <- function(sdf, step_label, description = "") {
  id_col <- .cb_env$id_col

  if (inherits(sdf, "tbl_spark")) {
    n_now <- sdf %>%
      select(dplyr::all_of(id_col)) %>%
      distinct() %>%
      sparklyr::sdf_nrow() %>%
      as.integer()
  } else {
    n_now <- length(unique(sdf[[id_col]]))
  }

  n_prev  <- if (nrow(.cb_env$tracking) == 0) n_now else dplyr::last(.cb_env$tracking$n_ids)
  removed <- n_prev - n_now

  .cb_env$tracking <- bind_rows(
    .cb_env$tracking,
    tibble(step = step_label, description = description,
           n_ids = n_now, n_removed = removed)
  )

  invisible(n_now)
}

#' Get the current tracking log as a tibble
#' @return A tibble with all tracking steps.
#' @export
track_get <- function() {
  .cb_env$tracking
}

#' Reset the tracking log
#' @return Invisible NULL.
#' @export
track_reset <- function() {
  .cb_env$tracking <- tibble(
    step = character(), description = character(),
    n_ids = integer(), n_removed = integer()
  )
  invisible(NULL)
}
