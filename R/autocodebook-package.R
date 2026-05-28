#' autocodebook: Automatic Codebook and Tracking for Spark/dplyr Pipelines
#'
#' Wraps dplyr verbs to automatically capture variable metadata (type, source
#' columns, categories, and source code), producing a codebook and an
#' eligibility tracking table with zero manual documentation. Works with both
#' sparklyr (tbl_spark) and local data frames.
#'
#' @keywords internal
#' @import rlang
#' @import dplyr
#' @import tibble
#' @import gt
#' @importFrom stats sd quantile setNames
#' @importFrom utils head write.csv
"_PACKAGE"
