#' Global variables
#' 
#' Declare variables used in dplyr pipelines as global
#' @keywords internal
#' @name utils-global
utils::globalVariables(
  c(
    # Original variables
    "human_bodycontour", "x", "y", "id", "in_system", 
    "value", "organ", "value_label", "temp_organ", 
    "temp_value", "organ_display", ".",
    
    # New variables from recent checks
    "x_mean", "x_sd", "y_mean", "y_sd", "X", "Y",
    "human_organ_coord", "mouse_bodycontour", "mouse_organ_coord",
    "organelle_bodycontour", "organelle_organ_coord", "has_data",
    "sd", "V1",
    
    # Data objects that are loaded from package data
    "human_organ_systems", "mouse_organ_systems",
    
    # dplyr and tidy evaluation variables
    ".data"
  )
)

