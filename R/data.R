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
human_organ_systems <- data.frame(  
  organ = c("heart", "artery", "vein", "capillary", "blood", "bone marrow","arm_blood_vessel", "thigh_blood_vessel",  
            "brain", "spinal cord", "nerve", "eye", "ear", 
            "lung", "trachea", "bronchus", "diaphragm", "pleura","nasopharyngeal",  
            "liver", "stomach", "small_intestine","large_intestine","pancreas", "esophagus","nasopharyngeal","tongue","gallbladder",  
            "kidney", "bladder", "ureter",  
            "skin", "hair", "nails",  
            "bone","cartilage", "ligament", "tendon", "muscle",  
            "spleen", "thymus","lymph node", "tonsil",  
            "testis", "ovary", "uterus", "prostate","breast","cervix"),  
  system = c(rep("circulatory", 8),  
             rep("nervous", 5),  
             rep("respiratory", 6),  
             rep("digestive", 9),  
             rep("urinary", 3),  
             rep("integumentary", 3),  
             rep("musculoskeletal", 5),  
             rep("lymphatic", 4),  
             rep("reproductive", 6)),  
  stringsAsFactors = FALSE  # Avoid issues caused by factor types  
)  


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
#'   \item{\code{V1}}{Integer. Original index from source data (reserved for compatibility).}
#'   \item{\code{id}}{Character. Group identifier for contour segments (used to draw connected regions).}
#'   \item{\code{x}}{Numeric. X-coordinate of contour points.}
#'   \item{\code{y}}{Numeric. Y-coordinate of contour points.}
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
mouse_organ_systems <- data.frame(  
  organ = c("heart","vessel","bone_marrow",  
            "brain","eye","nerve",  
            "lung","trachea",  
            "esophagus","stomach","small_intestine","large_intestine","liver","pancreas","tongue",  
            "kidney","bladder",  
            "skin",  
            "bone","muscle","bone_marrow",  
            "lymph_nodes","spleen","thymus","bone_marrow",  
            "ovary","testis","uterus",  
            "adrenal_gland","thyroid_gland","ovary","testis","pancreas"),  
  system = c(rep("circulatory", 3),  
             rep("nervous", 3),  
             rep("respiratory", 2),  
             rep("digestive", 7),  
             rep("urinary", 2),  
             rep("integumentary", 1),  
             rep("musculoskeletal", 3),  
             rep("lymphatic", 4),  
             rep("reproductive", 3),  
             rep("endocrine",5)),  
  stringsAsFactors = FALSE  # Avoid issues caused by factor types  
)  


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
#'   \item{\code{V1}}{Integer. Original index from source data (reserved for compatibility).}
#'   \item{\code{id}}{Character. Group identifier for contour segments (used to draw connected regions).}
#'   \item{\code{x}}{Numeric. X-coordinate of contour points.}
#'   \item{\code{y}}{Numeric. Y-coordinate of contour points.}
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






