#' Human Organ-system mapping data
#'
#' Defines the physiological system each organ belongs to, used for filtering organs by system.
#'
#' @format A data frame with the following columns:
#' \describe{
#'    \item{organ}{Organ name (character, standardized name)}
#'    \item{system}{Belonging system (character, e.g., "circulatory" for circulatory system)}
#' }
#' @export 
"human_organ_systems"

#' Human organ coordinate data
#'
#' Contains plotting coordinates (x, y) for each organ, used for drawing organ contours.
#'
#' @format A list where each element is a data frame containing:
#' \describe{
#'    \item{x}{x-coordinate (numeric)}
#'    \item{y}{y-coordinate (numeric)}
#'    \item{id}{Group ID (for drawing polygons, integer)}
#' }
"human_organ_coord"

#' Human body contour coordinate data
#'
#' Contains plotting coordinates for the human body contour, used for drawing the background outline.
#'
#' @format A data frame containing:
#' \describe{
#'    \item{\code{V1}}{Integer. Original index from source data (reserved for compatibility).}
#'    \item{\code{id}}{Character. Group identifier for contour segments (used to draw connected regions).}
#'    \item{\code{x}}{Numeric. X-coordinate of contour points.}
#'    \item{\code{y}}{Numeric. Y-coordinate of contour points.}
#' }
"human_bodycontour"

#' Mouse Organ-system mapping data
#'
#' Defines the physiological system each mouse organ belongs to, used for filtering organs by system in mouse visualization.
#'
#' @format A data frame with the following columns:
#' \describe{
#'    \item{organ}{Organ name (character, standardized name)}
#'    \item{system}{Belonging system (character, e.g., "circulatory" for circulatory system)}
#' }
#' @export 
"mouse_organ_systems"

#' Mouse organ coordinate data
#'
#' Contains plotting coordinates (x, y) for each mouse organ, used for drawing organ contours in mouse visualization.
#'
#' @format A list where each element is a data frame containing:
#' \describe{
#'    \item{x}{x-coordinate (numeric)}
#'    \item{y}{y-coordinate (numeric)}
#'    \item{id}{Group ID (for drawing polygons, integer)}
#' }
"mouse_organ_coord"

#' Mouse body contour coordinate data
#'
#' Contains plotting coordinates for the mouse body contour, used for drawing the background outline in mouse visualization.
#'
#' @format A data frame containing:
#' \describe{
#'    \item{\code{V1}}{Integer. Original index from source data (reserved for compatibility).}
#'    \item{\code{id}}{Character. Group identifier for contour segments (used to draw connected regions).}
#'    \item{\code{x}}{Numeric. X-coordinate of contour points.}
#'    \item{\code{y}}{Numeric. Y-coordinate of contour points.}
#' }
"mouse_bodycontour"

#' Organelle Body Contour Data
#'
#' A dataset containing the contour coordinates for organelle cell outline.
#'
#' @format A data frame with variables for cell contour coordinates
"organelle_bodycontour"

#' Organelle Organ Coordinate Data
#'
#' A list containing coordinate data for various organelles.
#'
#' @format A named list where each element is a data frame with organelle coordinates
"organelle_organ_coord"