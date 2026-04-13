# =============================================================================
# autocodebook — Estado global (environment interno do pacote)
# =============================================================================
# Usa um environment isolado para guardar codebook + tracking.
# Isso evita poluir o .GlobalEnv e permite múltiplas sessões
# independentes via cb_init().
# =============================================================================

#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows filter last

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
  n_removed   = integer()
)

.cb_env$id_col <- "id"

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
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' cb_init(id_col = "id_cidacs_pop100_v2")
cb_init <- function(id_col = "id") {
  .cb_env$id_col <- id_col
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
    n_removed   = integer()
  )
  message("[autocodebook] Sessão iniciada. ID col = '", id_col, "'")
  invisible(NULL)
}
