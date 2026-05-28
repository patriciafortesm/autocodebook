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
#' @param assume_unique Logical. If TRUE, skips the `distinct()` call when
#'   counting (use only when you are certain the ID column has no duplicates
#'   at this stage - e.g. after a deduplication step). Default: FALSE.
#'
#' @return Invisible integer: number of unique IDs.
#' @export
track_step <- function(sdf, step_label, description = "",
                       assume_unique = FALSE) {
  id_col <- .cb_env$id_col

  t0 <- Sys.time()

  if (inherits(sdf, "tbl_spark")) {
    if (isTRUE(assume_unique)) {
      n_now <- sdf %>%
        sparklyr::sdf_nrow() %>%
        as.integer()
    } else {
      n_now <- sdf %>%
        select(dplyr::all_of(id_col)) %>%
        distinct() %>%
        sparklyr::sdf_nrow() %>%
        as.integer()
    }
  } else {
    if (isTRUE(assume_unique)) {
      n_now <- nrow(sdf)
    } else {
      n_now <- length(unique(sdf[[id_col]]))
    }
  }

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  n_prev  <- if (nrow(.cb_env$tracking) == 0) n_now else dplyr::last(.cb_env$tracking$n_ids)
  removed <- n_prev - n_now

  .cb_env$tracking <- bind_rows(
    .cb_env$tracking,
    tibble(step = step_label, description = description,
           n_ids = n_now, n_removed = removed,
           elapsed_s = elapsed)
  )

  .cb_msg("[autocodebook] ", step_label, " -> n=", n_now,
          " (removidos: ", removed, ") | ",
          sprintf("%.2fs", elapsed))

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
    n_ids = integer(), n_removed = integer(),
    elapsed_s = numeric()
  )
  invisible(NULL)
}

# =============================================================================
# cb_checkpoint() — Materializa um tbl_spark para quebrar lazy evaluation
# =============================================================================

#' Checkpoint a Spark DataFrame
#'
#' Forces materialization of a lazy Spark plan. Useful in long pipelines where
#' query plans get too deep and the optimizer starts re-computing upstream
#' steps. For local data frames, this is a no-op.
#'
#' @param sdf A Spark DataFrame (tbl_spark) or local data frame.
#' @param name Optional. Name to register the checkpoint under (Spark only).
#'   If NULL, a temporary name is generated.
#' @param mode Character. One of `"memory"` (cache in memory, fastest),
#'   `"disk"` (sdf_checkpoint via disk, more durable), or `"register"`
#'   (just register as temp table without caching). Default: `"memory"`.
#'
#' @return The (possibly materialized) data frame.
#' @export
cb_checkpoint <- function(sdf, name = NULL, mode = c("memory", "disk", "register")) {
  mode <- match.arg(mode)

  if (!inherits(sdf, "tbl_spark")) {
    # No-op para local
    return(sdf)
  }

  t0 <- Sys.time()

  if (is.null(name)) {
    name <- paste0("cb_chk_", format(Sys.time(), "%H%M%S"),
                   "_", sample.int(9999, 1))
  }

  out <- switch(mode,
    memory   = sparklyr::sdf_register(sdf, name = name) %>%
                 sparklyr::sdf_persist(storage.level = "MEMORY_AND_DISK"),
    disk     = sparklyr::sdf_checkpoint(sdf, eager = TRUE),
    register = sparklyr::sdf_register(sdf, name = name)
  )

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  .cb_msg("[autocodebook] checkpoint (", mode, ") '", name, "' | ",
          sprintf("%.2fs", elapsed))

  out
}
