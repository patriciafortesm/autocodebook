# =============================================================================
# autocodebook — Estado global (environment interno do pacote)
# =============================================================================
# Usa um environment isolado para guardar codebook + tracking.
# Isso evita poluir o .GlobalEnv e permite multiplas sessoes
# independentes via cb_init().
# =============================================================================


# Environment interno — armazena os logs
.cb_env <- new.env(parent = emptyenv())

.cb_env$codebook <- tibble(
  variable   = character(),
  type       = character(),
  source     = character(),
  label      = character(),
  categories = character(),
  code       = character(),
  block      = character()
)

.cb_env$tracking <- tibble(
  step        = character(),
  description = character(),
  n_ids       = integer(),
  n_removed   = integer(),
  elapsed_s   = numeric()
)

# Defaults (v0.1.0 compat)
.cb_env$id_col        <- "id"
.cb_env$verbose       <- FALSE
.cb_env$default_cache <- FALSE
.cb_env$flow          <- NULL

# =============================================================================
# cb_init() — Inicializa/reseta o estado para um novo pipeline
# =============================================================================

#' Initialize autocodebook session
#'
#' Resets the codebook and tracking logs and sets the ID column
#' used for counting unique individuals in track_step().
#'
#' @param id_col Character. Name of the unique identifier column.
#'   Default: "id".
#' @param verbose Logical. If TRUE, prints diagnostic messages from
#'   track_step(), auto_filter(), and cb_checkpoint(). Default: FALSE
#'   (matches v0.1.0 behavior - silent).
#' @param default_cache Logical. If TRUE, big-data verbs cache intermediate
#'   results in Spark by default. Can be overridden per-call. Default: FALSE.
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' cb_init(id_col = "id_cidacs_pop100_v2")
#' cb_init(id_col = "id", verbose = TRUE, default_cache = TRUE)
cb_init <- function(id_col = "id", verbose = FALSE, default_cache = FALSE) {
  .cb_env$id_col        <- id_col
  .cb_env$verbose       <- isTRUE(verbose)
  .cb_env$default_cache <- isTRUE(default_cache)

  .cb_env$codebook <- tibble(
    variable   = character(),
    type       = character(),
    source     = character(),
    label      = character(),
    categories = character(),
    code       = character(),
    block      = character()
  )
  .cb_env$tracking <- tibble(
    step        = character(),
    description = character(),
    n_ids       = integer(),
    n_removed   = integer(),
    elapsed_s   = numeric()
  )
  # Inicializa a arvore de fluxo (CONSORT)
  if (exists(".flow_init", mode = "function")) .flow_init()
  message("[autocodebook] Sessao iniciada. ID col = '", id_col,
          "' | verbose = ", .cb_env$verbose,
          " | default_cache = ", .cb_env$default_cache)
  invisible(NULL)
}

# =============================================================================
# Setters individuais (uteis para ligar/desligar durante o pipeline)
# =============================================================================

#' Toggle verbose diagnostic messages
#'
#' Controls whether track_step(), auto_filter() and cb_checkpoint() print
#' diagnostic messages (n removed, elapsed time, etc.).
#'
#' @param verbose Logical.
#' @return Invisible previous value.
#' @export
cb_set_verbose <- function(verbose = TRUE) {
  old <- .cb_env$verbose
  .cb_env$verbose <- isTRUE(verbose)
  invisible(old)
}

#' Toggle default caching for big-data verbs
#'
#' @param default_cache Logical.
#' @return Invisible previous value.
#' @export
cb_set_default_cache <- function(default_cache = TRUE) {
  old <- .cb_env$default_cache
  .cb_env$default_cache <- isTRUE(default_cache)
  invisible(old)
}

# Helper interno: mensagem so se verbose
.cb_msg <- function(...) {
  if (isTRUE(.cb_env$verbose)) message(...)
}
