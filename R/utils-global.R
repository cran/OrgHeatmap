#' Global variables
#' 
#' Declare variables used in dplyr pipelines as global
#' @keywords internal
#' @name utils-global
utils::globalVariables(
  c(
    "human_bodycontour", "x", "y", "id", "in_system", 
    "value", "organ", "value_label", "temp_organ", 
    "temp_value", "organ_display", "."
  )
)