#' Visualization Tool for Human, Mouse and Organelle Data
#' 
#' This tool visualizes numerical data (such as gene expression) on a human, mouse or organelle map.
#' It supports custom color schemes, organ system filtering, and bar charts for quantitative comparison.
#' 
#' @param data Data frame with at least two columns: organ name and corresponding value
#' @param species Character, species to visualize, one of \code{"human"} (default), \code{"mouse"}, or \code{"organelle"}. 
#'   Determines the default body/cell contour, organ coordinates, and organ-system mapping.
#' @param system Optional character vector specifying organ system to display(not applicable for organelles)
#' @param valid_organs Optional character vector of valid organ names for filtering
#' @param sort_by_value Logical, default TRUE, sorts by value descending
#' @param title Optional character vector for plot title
#' @param showall Logical, default FALSE. If TRUE, shows all organ outlines (grey) with light grey fill (#EFEFEF) for non-target organs (to provide anatomical context).
#' @param outline Logical, default TRUE, draws human/cell outline
#' @param palette Character, name of RColorBrewer palette (e.g., "YlOrRd", "PuBuGn") for unified color scheme (applies to both organ heatmap and bar chart if no custom colors are specified). 
#'   Ignored if `color_low`/`color_high` (for heatmap) or `organbar_low`/`organbar_high` (for bar chart) are specified. Default: "YlOrRd" (suitable for highlighting high values).
#' @param color_low Character, custom color for the **minimum value** of the organ heatmap (and bar chart if `organbar_low` is not specified). Overrides `palette` but is overridden by `organbar_low` (highest priority). Default: NULL.
#' @param color_high Character, custom color for the **maximum value** of the organ heatmap (and bar chart if `organbar_high` is not specified). Overrides `palette` but is overridden by `organbar_high` (highest priority). Default: NULL.
#' @param color_mid Character, optional color for the **middle value** of the organ heatmap (for 3-color gradients). Default: NULL.
#' @param reverse_palette Logical, whether to reverse the color order of `palette`. Default: FALSE (low=light, high=dark).
#' @param fillcolor_outline Character, default "#F5D5B8" for human/mouse, "#F0F8FF" for organelle, fill color for outline
#' @param fillcolor_organ Character, fallback color scheme for organs (supports viridis options: "viridis", "plasma", "magma", etc.). Only used if no `palette`, `color_low/color_high`, or `organbar_low/organbar_high` are specified. Default: "plasma".
#' @param fillcolor_other Character, default "#D3D3D3", fill color for non-target organelles
#' @param organbar Logical, default TRUE, shows value bar chart
#' @param organbar_title Optional character, title for bar chart legend
#' @param organbar_digit Integer, default 4, digits for bar values
#' @param organbar_color Optional character, solid color for bars
#' @param organbar_low Character, low end of gradient for **bar chart** (and organ heatmap if `color_low` is not specified). Highest priority for color configuration (overrides `color_low` and `palette`). Default: NULL.
#' @param organbar_high Character, high end of gradient for **bar chart** (and organ heatmap if `color_high` is not specified). Highest priority for color configuration (overrides `color_high` and `palette`). Default: NULL.
#' @param direction Integer, default 1. Direction of color gradient: 1 = normal (low value → light color, high value → dark color); -1 = reversed (low value → dark color, high value → light color).
#' @param save_clean_data Logical, default FALSE, saves cleaned data
#' @param save_plot Logical, default FALSE, whether to save the plot
#' @param plot_path Character, default `file.path(getwd(), "organ_plot.png")`, path for saving the plot (default to current working directory)
#' @param clean_data_path Character, default `file.path(getwd(), "clean_data.rds")`, path for cleaned data (default to current working directory)
#' @param plot_width Numeric, default 10, plot width in inches
#' @param plot_height Numeric, default 8, plot height in inches
#' @param plot_dpi Numeric, default 300, plot resolution
#' @param plot_device Character, default "png", plot format (e.g., "png", "pdf")
#' @param organ_name_mapping Optional: Either a named vector (non-standard → standard names, e.g., c("adrenal" = "adrenal_gland")), 
#'   a data frame (must contain columns specified by `original_col` and `standard_col`), or a CSV path (same column requirement). 
#'   Internally processed by `create_organ_mapping()` for standardization (lowercase, underscores for spaces).
#' @param organ_system_map Data frame, CSV path, or NULL (default). If NULL, uses species-specific defaults:
#'   \code{human_organ_systems} for humans, \code{mouse_organ_systems} for mice, or \code{organelle_systems} for organelles.
#' @param aggregate_method Character, aggregation method for duplicate organs 
#'   (one of "mean" \code{default}, "sum", "count").
#' @param organ_col Character, default "organ", column name for organs
#' @param value_col Character, default "value", column name for values
#'
#' @return List containing:
#'   \item{plot}{ggplot2 object}
#'   \item{clean_data}{Cleaned data frame}
#'   \item{system_used}{Organ system used}
#'   \item{mapped_organs}{Standardized organ names}
#'   \item{missing_organs}{Organs without coordinates}
#'   \item{total_value}{Sum of all values}
#'
#' @examples
#' \donttest{
#' # Load the package
#' library(OrgHeatmap)
#' 
#' # Note: Example datasets (example_Data1, example_Data2, example_Data3, expr_data)
#' # are included in the package's 'extdata' directory.
#' 
#' ## Load example data files from extdata (contains example_Data1, example_Data2, example_Data3)
#'data_path <- system.file("extdata", "exampledata.Rdata", package = "OrgHeatmap")
#'load(data_path)  
#' 
#' # 1.Plot all organs and save results using internal saving function
#'result_all <- OrgHeatmap(
#'   example_Data3, 
#'   organbar = TRUE,
#'   save_plot = TRUE,  # Enable plot saving
#'   plot_path = file.path(tempdir(), "all_system.png"),  
#'   plot_width = 10,
#'   plot_height = 8,
#'   save_clean_data = TRUE,  # Enable cleaned data saving
#'   clean_data_path = file.path(tempdir(), "all_system_clean_data.rds")
#' )
#' print(result_all$plot)  # Print the plot to the console
#' 
#' # 2. Plot circulatory system organs and save results
#' result_circulatory <- OrgHeatmap(
#'   example_Data3, 
#'   system = "circulatory",
#'  organbar = TRUE,
#'   save_plot = TRUE,
#'  plot_path = file.path(tempdir(), "circulatory_system.png"),
#'  plot_width = 10,
#'   plot_height = 8,
#'   plot_device = "png",  # Specify plot format
#'   save_clean_data = TRUE,
#'   clean_data_path = file.path(tempdir(), "circulatory_clean_data.rds")
#' )
#' print(result_circulatory$plot)  # Print the plot to the console
#' 
#' # 3. Quick color configuration with palette
#' # Core logic: Trigger internal color_config generation via palette parameters,
#' # ensuring organ and bar chart colors are synchronized
#' result_palette <- OrgHeatmap(
#'example_Data3,
#'system = "respiratory",  # Respiratory system
#'palette = "PuBuGn",      # Use RColorBrewer's blue-purple-green palette
#'reverse_palette = TRUE,  # Reverse palette (low value = dark green, high value = purple)
#'color_mid = "#87CEEB",   # Custom middle color (sky blue)
#'title = "Respiratory System (Palette: PuBuGn)",
#'organbar_title = "Mean Value",
#'organbar_digit = 2,
#'showall = TRUE,
#'save_plot = TRUE,
#'plot_path = file.path(tempdir(),"respiratory_palette.png")
#'   # To use solid color for bars, add parameter: organbar_color = "skyblue"
#'   # (overrides gradient and synchronizes with organ colors)
#')
#' print(result_palette$plot)
#' 
#' 
#'  ## Load the example dataset expr_data from extdata
#'  expr_data_path <- system.file("extdata", "expr_data.rds", package = "OrgHeatmap")
#'  expr_data <- readRDS(expr_data_path)
#' 
#' # Custom organ mapping (standardize original organ names)
#' custom_mapping <- c(
#'   "adrenal" = "adrenal_gland",
#'   "lymph node" = "lymph_node",
#'   "soft tissue" = "muscle",
#'   "peritoneal" = "peritoneum"
#' )
#' 
#' # Add prostate cancer-specific organs to system mapping
#' prostate_organ_systems <- rbind(
#'   human_organ_systems,  # Package's built-in organ system mapping
#'   data.frame(
#'     organ = c("prostate", "bone", "lymph_node", "adrenal_gland"),
#'     system = c("reproductive", "musculoskeletal", "lymphatic", "endocrine"),
#'     stringsAsFactors = FALSE
#'   )
#' )
#' 
#' # Generate TP53 expression heatmap and save results
#' tp53_plot <- OrgHeatmap(
#'   data = expr_data,
#'   value_col = "expression",  # Specify value column name as "expression"
#'   organ_system_map = prostate_organ_systems,  # Use custom organ system mapping
#'   organ_name_mapping = custom_mapping,  # Apply organ name mapping
#'   title = "TP53 Expression in Metastatic Prostate Cancer",
#'   organbar_title = "Mean Expression(log2) of TP53",
#'   aggregate_method = "mean",  # Calculate mean by organ
#'   showall = TRUE,  # Show all organ outlines
#'   fillcolor_other = "#DCDCDC",  # Fill color for non-target organs
#'   organbar_digit = 2,  # Keep 2 decimal places for bar values
#'   direction = -1,  # Reverse color gradient (darker = higher expression)
#'   save_plot = TRUE,  # Save the plot
#'   plot_path = file.path(tempdir(), "tp53_expression_metastatic_prostate.png"),
#'   plot_width = 14,
#'   plot_height = 10,
#'   plot_dpi = 300,
#'   save_clean_data = TRUE,  # Save cleaned data
#'   clean_data_path = file.path(tempdir(), "tp53_clean_data.rds")
#' )
#' 
#' # Print the plot
#' print(tp53_plot$plot)
#' 
#' # 4.Plot mouse digestive system
#' # Load mouse example data (included in the package)
#' mouse_data_path <- system.file("extdata", "exampledata.Rdata", package = "OrgHeatmap")
#' load(mouse_data_path)
#' 
#' # Generate plot for mouse digestive system
#' mouse_digestive_plot <- OrgHeatmap(
#' data = example_Data1,
#' species = "mouse",  # Specify mouse species
#' system = "digestive",
#' organbar = TRUE,
#' palette = "PuBu",
#' save_plot = TRUE,
#' plot_path = file.path(tempdir(), "mouse_digestive_plot.png"),
#' save_clean_data = TRUE
#' )
#' print(mouse_digestive_plot$plot)
#' }
#'
#'  # 5. Plot organelles 
#' organelle_data <- data.frame(
#'  organ = c("mitochondrion", "nucleus", "endoplasmic_reticulum","cell_membrane"),
#'  value = c(15.2, 8.7, 6.3,6.8)
#')
#'
#' organelle_plot <- OrgHeatmap(
#'  data = organelle_data,
#'  species = "organelle",
#' title = "Organelle Expression Visualization",
#' organbar_title = "Expression Level",
#' save_plot = TRUE,
#' plot_path = file.path(tempdir(), "organelle_expression_plot.png")
#')
#'
#' @details 
#' The function uses \code{get_component_colors()} (an internal helper function) to generate unified color schemes:
#' 1. If `organbar_low` and `organbar_high` are specified by the user, they will be used directly (highest priority);
#' 2. If not, colors are generated from the `palette` (RColorBrewer) with optional reversal (`reverse_palette`);
#' 3. Custom middle color (`color_mid`) is supported for 3-color gradients (applied to both heatmap and bar chart).
#' For valid `palette` names, see `RColorBrewer::brewer.pal.info`.
#' 
#' ## Organ Mapping Logic
#' - For \code{organ_name_mapping}: Accepts a named vector, data frame, or CSV path. 
#'   Internal helper `create_organ_mapping()` standardizes names (lowercase, underscores for spaces).
#' - For \code{organ_system_map}: Custom tables (data frame/CSV) are processed to align with 
#'   built-in `human_organ_systems` (for humans), `mouse_organ_systems` (for mice), or `organelle_systems` (for organelles) 
#'   format via `create_organ_mapping()`.
#' 
#' @importFrom dplyr rename select filter mutate group_by summarise left_join distinct
#' @importFrom ggplot2 ggplot aes theme_void ggtitle theme element_text scale_fill_gradient2
#'   scale_fill_viridis_c scale_fill_distiller labs guides scale_y_discrete geom_tile geom_text GeomPolygon
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all str_to_title
#' @importFrom patchwork plot_layout
#' @importFrom data.table rbindlist
#' @importFrom ggpolypath geom_polypath
#' @importFrom RColorBrewer brewer.pal
#' @importFrom magrittr %>%
#' @importFrom rlang := sym .data
#' @importFrom ggplot2 margin scale_fill_gradient
#' @importFrom dplyr all_of n
#' @importFrom utils read.delim
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @importFrom sf st_as_sf st_combine st_cast st_sf st_coordinates
#' @importFrom stringdist stringdist
#' @importFrom viridis viridis
#' @export
OrgHeatmap <- function(data,
                       species = c("human", "mouse", "organelle"),
                       system = NULL,
                       valid_organs = NULL,
                       sort_by_value = TRUE,
                       title = NULL,
                       showall = FALSE,
                       outline = TRUE,
                       palette = "YlOrRd",         # Palette name (RColorBrewer)
                       color_high = NULL,           # Custom color for heatmap maximum(overrides palette)                        
                       color_low = NULL,           # Custom color for heatmap minimum (overrides palette)
                       color_mid = NULL,            # Custom color for heatmap middle (for 3-color gradients)
                       reverse_palette = FALSE,    # Whether to reverse the color order of `palette` (default: FALSE)
                       fillcolor_outline = NULL, 
                       fillcolor_organ = "plasma",
                       fillcolor_other = "#D3D3D3",
                       organbar = TRUE,
                       organbar_title = NULL,
                       organbar_digit = 4,
                       organbar_color = NULL,
                       organbar_low = NULL,
                       organbar_high = NULL,
                       direction = 1,
                       save_clean_data = FALSE,
                       save_plot = FALSE,
                       clean_data_path = file.path(getwd(), "clean_data.rds"),  
                       plot_path = file.path(getwd(),"organ_plot.png"),        
                       plot_width = 10,
                       plot_height = 8,
                       plot_dpi = 300,
                       plot_device = "png",
                       organ_system_map = NULL,
                       organ_name_mapping = NULL,
                       aggregate_method = "mean",
                       organ_col = "organ",
                       value_col = "value"
) {  
  
  # Check all dependency packages (provide clear installation prompts if missing)
  check_dependency <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required. Install with: install.packages('", pkg, "')"))
    }
  }
  
  # Core dependency package list (covers all packages used in the function)
  deps <- c(
    "sf", "dplyr", "stringdist", "ggpolypath", "patchwork", 
    "viridis", "data.table", "stringr", "RColorBrewer", 
    "ggplot2", "stats", "purrr", "grDevices"
  )
  invisible(lapply(deps, check_dependency))
  
  close_body_contour <- function(contour_data) {
    # 1. Filter outliers (remove coordinates outside reasonable range to avoid line jumps)
    contour_clean <- contour_data %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(
        x_mean = mean(x, na.rm = TRUE),
        x_sd = sd(x, na.rm = TRUE),
        y_mean = mean(y, na.rm = TRUE),
        y_sd = sd(y, na.rm = TRUE)
      ) %>%
      # Keep points within mean ± 3 standard deviations (filter extreme outliers)
      dplyr::filter(
        x >= x_mean - 3*x_sd, x <= x_mean + 3*x_sd,
        y >= y_mean - 3*y_sd, y <= y_mean + 3*y_sd
      ) %>%
      dplyr::select(-x_mean, -x_sd, -y_mean, -y_sd) %>%
      dplyr::ungroup()
    
    # 2. Group by id and process point sorting and contour closing for each segment
    contour_processed <- contour_clean %>%
      dplyr::group_by(id) %>%
      dplyr::group_modify(function(sub, key) {
        # Skip segments with too few points (cannot form a polygon)
        if (nrow(sub) < 3) {
          warning(paste0("Mouse contour segment [id=", sub$id[1], "] has fewer than 3 points, skipped"))
          return(NULL)
        }
        
        # Sort points (rearrange clockwise to fix line crossing)
        coords_sf <- sub %>%
          sf::st_as_sf(coords = c("x", "y")) %>%  # Convert to sf spatial object
          sf::st_combine() %>%                   # Combine points into a geometry collection
          sf::st_cast("POLYGON") %>%             # Convert to polygon (automatically sorts points)
          sf::st_sf()                           # Convert to sf data frame
        
        # Extract sorted x/y coordinates
        sorted_coords <- sf::st_coordinates(coords_sf)[, 1:2] %>%
          as.data.frame() %>%
          dplyr::rename(x = X, y = Y) %>%
          dplyr::mutate(
            id = sub$id[1], # Retain segment id
            V1 = 1:nrow(.)   # Regenerate index (avoid original index chaos)
          )
        
        # 3. Close the polygon (ensure first and last points are the same)
        if (sorted_coords$x[1] != sorted_coords$x[nrow(sorted_coords)] ||
            sorted_coords$y[1] != sorted_coords$y[nrow(sorted_coords)]) {
          sorted_coords <- dplyr::bind_rows(sorted_coords, sorted_coords[1, ])
        }
        
        return(sorted_coords)
      }) %>%
      dplyr::ungroup()
    
    # Check if processed data is empty
    if (nrow(contour_processed) == 0) {
      stop("Mouse contour data is empty after processing, please check the validity of the original coordinate data")
    }
    
    return(contour_processed)
  }
  
  # Validate and lock the species parameter (prevent invalid inputs)
  species <- match.arg(species)
  
  if (is.null(fillcolor_outline)) {
    fillcolor_outline <- if(species == "organelle") "#F0F8FF" else "#F5D5B8"
  }
  
  # Define core data list by species (human + mouse + organelle)
  core_data_list <- list(
    human = c("human_bodycontour", "human_organ_coord", "human_organ_systems"),
    mouse = c("mouse_bodycontour", "mouse_organ_coord", "mouse_organ_systems"),
    organelle = c("organelle_bodycontour", "organelle_organ_coord")
  )
  
  # Get core data to check for the current species
  required_data <- core_data_list[[species]]
  
  # Check if core data exists (prioritize package namespace, then global environment)
  missing_data <- sapply(required_data, function(data_name) {
    !exists(data_name, envir = .GlobalEnv)
  })
  
  # Terminate with error if any data is missing (unified error message format)
  if (any(missing_data)) {
    stop(paste0(
      "Missing required ", species, " data: ", 
      paste(names(missing_data)[missing_data], collapse = ", "), "\n",
      "Ensure data is loaded to the global environment (e.g., via load('data_path.Rdata')).")
    )
  }
  
  current_default_system <- NULL
  
  # Load species-specific data
  if (species == "human") {
    current_bodycontour <- human_bodycontour    # Human body outline coordinates
    current_organ_coord <- human_organ_coord    # Human organ coordinates (list)
    current_default_system <- human_organ_systems     # Default human organ-system mapping
  } else if (species == "mouse") {
    # Load raw mouse contour data
    current_bodycontour_raw <- mouse_bodycontour
    # Process contour: sort points + close + filter outliers
    current_bodycontour <- close_body_contour(current_bodycontour_raw)
    # Load mouse organ data
    current_organ_coord <- mouse_organ_coord    # Mouse organ coordinates (list)
    current_default_system <- mouse_organ_systems  # Default mouse organ-system mapping
  } else { # species == "organelle"
    current_bodycontour <- organelle_bodycontour    # Cell outline coordinates
    current_organ_coord <- organelle_organ_coord    # Organelle coordinates (list)
  }
  
  # Set default organ_system_map if user doesn't provide one
  if (is.null(organ_system_map)) {
    if (species == "organelle") {
      organ_system_map <- NULL 
    } else {
      organ_system_map <- current_default_system
    }
  }
  
  # Validate body/cell contour data (must contain 'x' and 'y' columns)
  if (!all(c("x", "y") %in% colnames(current_bodycontour))) {
    stop(paste0(
      "Invalid ", species, " body/cell contour data: missing 'x' or 'y' column.\n",
      "Ensure current_bodycontour has columns 'x' (x-coordinate) and 'y' (y-coordinate)."
    ))
  }
  
  # Validate organ/organelle coordinate data (each must contain 'x', 'y', and 'id' columns; 'id' for grouping in plotting)
  organ_coord_valid <- sapply(current_organ_coord, function(org_df) {
    all(c("x", "y", "id") %in% colnames(org_df))
  })
  if (!all(organ_coord_valid)) {
    invalid_organs <- paste(names(organ_coord_valid)[!organ_coord_valid], collapse = ", ")
    stop(paste0(
      "Invalid ", species, " organ/organelle coordinate data: missing 'x', 'y', or 'id' column in:\n",
      invalid_organs, "\nEnsure each organ data frame in current_organ_coord has these 3 columns."
    ))
  }
  
  # Helper function: standardize organ name format (space to underscore, lowercase)
  standardize_organ_name <- function(name) {
    tolower(gsub("\\s+", "_", name))
  }
  
  # Preprocess organ system mapping (standardize names)
  preprocess_organ_system_map <- function(map) {
    
    if (is.null(map)) {
      return(NULL)
    }
    
    # If a CSV file path is provided, automatically read it as a data frame
    if (is.character(map) && file.exists(map)) {
      # Read CSV (comma-separated by default; adjust sep parameter if needed)
      map <- read.csv(map, stringsAsFactors = FALSE, check.names = FALSE)
      # Verify that the resulting data frame contains required columns
      if (!all(c("organ", "system") %in% colnames(map))) {
        stop("CSV file must contain 'organ' and 'system' columns (column names must match exactly)")
      }
    }
    
    map %>%
      dplyr::mutate(
        organ = standardize_organ_name(organ),
        system = tolower(system)
      )
  }
  
  # Standardize coordinate data names
  standardize_coord_names <- function(coord_list) {
    names(coord_list) <- standardize_organ_name(names(coord_list))
    coord_list
  }
  
  # Apply name standardization
  organ_system_map <- preprocess_organ_system_map(organ_system_map)
  current_organ_coord <- standardize_coord_names(current_organ_coord)
  
  current_organ_coord <- lapply(current_organ_coord, function(org_df) {
    if (!is.numeric(org_df$x) || !is.numeric(org_df$y)) {
      stop(paste0("Organ '", names(current_organ_coord)[which(sapply(current_organ_coord, identical, org_df))], 
                  "' coordinate 'x' or 'y' is not numeric. Ensure x/y are numeric."))
    }
    org_df
  })
  
  # Validate direction parameter
  if (!direction %in% c(1, -1)) {
    stop("direction must be 1 (default) or -1.")
  }
  
  # Validate organbar_digit
  if (organbar_digit < 1) {
    warning("organbar_digit must be at least 1. Setting to default value 1.")
    organbar_digit <- 1
  }
  
  # Check if user-specified columns exist
  if (!organ_col %in% colnames(data)) {
    stop(paste0("Specified organ column '", organ_col, "' not found in data"))
  }
  if (!value_col %in% colnames(data)) {
    stop(paste0("Specified value column '", value_col, "' not found in data"))
  }
  
  create_organ_mapping <- function(
    mapping_table,
    original_col = "original_name",
    standard_col = "standard_name",
    output_type = "name_mapping",
    sep = ","
  ) {
    # Input validation
    if (is.character(mapping_table) && length(mapping_table) == 1 && file.exists(mapping_table)) {
      # Read CSV (supports user-specified separator 'sep')
      mapping_table <- read.csv(
        mapping_table,
        stringsAsFactors = FALSE,
        sep = sep,
        check.names = FALSE  # Preserve original CSV column names to avoid automatic conversion
      )
    }
    
    if (!output_type %in% c("name_mapping", "system_mapping")) {
      stop("output_type must be 'name_mapping' or 'system_mapping'")
    }
    
    if (is.character(mapping_table)) {
      # Case 1: If it's a named vector (has names and length > 1), use directly as mapping rules
      if (!is.null(names(mapping_table)) && length(mapping_table) > 1) {
        mapping_df <- data.frame(
          original_name = names(mapping_table),
          standard_name = as.character(mapping_table),
          stringsAsFactors = FALSE
        )
      } 
      # Case 2: If it's a single character string, treat as file path
      else if (length(mapping_table) == 1) {
        if (!file.exists(mapping_table)) {
          stop("File not found: ", mapping_table)
        }
        mapping_df <- read.delim(mapping_table, sep = sep, stringsAsFactors = FALSE)
      } 
      # Case 3: Invalid character input (short vector without names)
      else {
        stop("Invalid character input for mapping_table. Use a named vector or file path.")
      }
    } 
    # Handle data frame input
    else if (is.data.frame(mapping_table)) {
      mapping_df <- mapping_table
    } 
    else {
      stop("mapping_table must be a named vector, data frame, or CSV file path")
    }
    
    if (nrow(mapping_df) == 0) stop("mapping_table is empty")
    
    # Generate name mapping (non-standard → standard)
    if (output_type == "name_mapping") {
      if (!all(c(original_col, standard_col) %in% colnames(mapping_df))) {
        stop("Missing columns: ", paste(setdiff(c(original_col, standard_col), colnames(mapping_df)), collapse = ", "))
      }
      mapping_df <- mapping_df[!is.na(mapping_df[[original_col]]) & !is.na(mapping_df[[standard_col]]), ]
      return(stats::setNames(
        sapply(mapping_df[[standard_col]], standardize_organ_name),  # Standardize target names to match organ coordinate naming rules
        mapping_df[[original_col]]
      ))
    } else {
      # Generate organ-system mapping
      if (!all(c("organ", "system") %in% colnames(mapping_df))) {
        stop("system_mapping requires 'organ' and 'system' columns")
      }
      return(dplyr::mutate(mapping_df,
                           organ = .data$organ, # Directly retain the original organ column (which has been standardized in preprocess_organ_system_map).
                           system = tolower(.data$system)
      ) %>% dplyr::distinct(.data$organ, .keep_all = TRUE))
    }
  }
  
  process_organ_data <- function(data, organ_col, value_col, organ_name_mapping, aggregate_method,current_organ_coord) {
    # 1. Input validation: Prevent invalid input from entering the pipeline
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame")
    }
    if (nrow(data) == 0) {
      stop("Input data cannot be empty")
    }
    if (!organ_col %in% colnames(data)) {
      stop(paste0("organ_col '", organ_col, "' not found in data"))
    }
    if (!value_col %in% colnames(data)) {
      stop(paste0("value_col '", value_col, "' not found in data"))
    }
    if (!aggregate_method %in% c("count", "sum", "mean")) {
      stop("aggregate_method must be one of 'count', 'sum', 'mean'")
    }
    
    fuzzy_matches <- character(0)
    
    # 2. Data processing pipeline 
    result <- data %>%
      # Rename columns and keep required columns
      dplyr::rename(
        temp_organ = all_of(organ_col),
        temp_value = all_of(value_col)
      ) %>%
      dplyr::select(temp_organ, temp_value) %>%  
      
      # Ensure conditional branches always return data frame (avoid NULL)
      {
        # Only perform mapping when organ_name_mapping is a character vector, otherwise return original data
        if (!is.null(organ_name_mapping) && is.character(organ_name_mapping)) {
          dplyr::mutate(., 
                        temp_organ = ifelse(
                          temp_organ %in% names(organ_name_mapping),  # Simplified: can directly reference column names in pipeline
                          organ_name_mapping[temp_organ], 
                          temp_organ
                        )
          )
        } else {
          .  # Return current data frame when condition not met, instead of NULL
        }
      } %>%
      
      # Standardize organ names
      dplyr::mutate(temp_organ = standardize_organ_name(temp_organ)) %>%
      
      # Fuzzy matching 
      dplyr::mutate(temp_organ = purrr::map_chr(temp_organ, function(x) {
        all_organs <- names(current_organ_coord)  
        if (x %in% all_organs) return(x)
        
        # Check if stringdist is available
        if (!requireNamespace("stringdist", quietly = TRUE)) {
          warning("Package 'stringdist' is required for fuzzy matching. Please install it.")
          return(x)
        }
        
        # Calculate string distances
        distances <- stringdist::stringdist(x, all_organs)
        if (any(is.na(distances))) {
          warning("Fuzzy matching failed for organ '", x, "' (NA distances)")
          return(NA_character_)
        }
        
        # Matching threshold (distance <= 2)
        min_dist <- min(distances)
        if (min_dist <= 2) {
          matched <- all_organs[which.min(distances)]
          fuzzy_matches <<- c(fuzzy_matches, paste0("'", x, "' -> '", matched, "'"))
          message(paste0("Fuzzy matched: '", x, "' -> '", matched, "'"))
          return(matched)
        } else {
          return(x)  # Retain original value when no match
        }
      })) %>%
      
      # Output all fuzzy match results at once after the fuzzy matching loop (single message)
      {
        if (length(fuzzy_matches) > 0) {
          message(paste(fuzzy_matches, collapse = "\n"))  # Separate multiple matches with newlines
        }
        .  # Return the data frame without interrupting the pipeline
      } %>%
      
      # Filter invalid values (moved after fuzzy matching to avoid premature filtering)
      dplyr::filter(!is.na(temp_organ), !is.na(temp_value)) %>%
      dplyr::mutate(temp_value = as.numeric(temp_value)) %>%
      dplyr::filter(!is.na(temp_value)) %>%  # Filter values that failed numeric conversion
      
      # Aggregate data (ensure at least one row remains after grouping)
      {
        if (nrow(.) == 0) {
          warning("No valid data remaining after filtering. Returning empty data frame.")
          .  # Return empty data frame instead of breaking
        } else {
          dplyr::group_by(., temp_organ) %>%
            dplyr::summarise(
              temp_value = switch(
                aggregate_method,
                "count" = dplyr::n(),
                "sum" = sum(temp_value, na.rm = TRUE),
                "mean" = mean(temp_value, na.rm = TRUE)
              ),
              .groups = "drop"
            )
        }
      } %>%
      
      # Restore user-specified column names
      dplyr::rename(
        !!organ_col := temp_organ,
        !!value_col := temp_value
      )
    
    all_processed_organs <- unique(result[[organ_col]])
    valid_organs <- names(current_organ_coord)
    
    missing_organs <- setdiff(all_processed_organs, valid_organs)
    
    clean_data <- result[result[[organ_col]] %in% valid_organs, ]
    
    if (nrow(clean_data) == 0) {
      warning("No organs with coordinate data found after filtering.")
    }
    
    return(list(clean_data = clean_data, missing_organs = missing_organs))
  }
  
  
  # Get organ coordinates function
  get_organ_coordinates <- function(organs) {
    valid_organs <- list()
    missing_organs <- character()
    standardized_organs <- standardize_organ_name(organs)
    
    # For organelles, get all available organelles, not just the ones in user data
    if (species == "organelle") {
      target_organs <- unique(c(standardized_organs, names(current_organ_coord)))
    } else {
      target_organs <- unique(standardized_organs)
    }
    
    for (org in target_organs) {
      if (org %in% names(current_organ_coord)) {  
        df <- current_organ_coord[[org]]  
        df$organ <- org
        valid_organs[[org]] <- df
      } else {
        missing_organs <- c(missing_organs, org)
      }
    }
    
    if (length(missing_organs) > 0 && species != "organelle") {
      warning(paste(
        "The following organs/organelles have no coordinate data:",
        paste(unique(missing_organs), collapse = ", ")
      ))
    }
    
    dplyr::bind_rows(valid_organs)
  }
  
  
  # Create bar chart data function
  create_organ_bar_data <- function(system_organs_data, organ_col, value_col) {
    system_organs_data %>% 
      dplyr::distinct(!!sym(organ_col), !!sym(value_col)) %>% 
      dplyr::mutate(
        organ_display = stringr::str_replace_all(!!sym(organ_col), "_", " ") %>% 
          stringr::str_to_title(),
        organ_display = ifelse(
          duplicated(organ_display) | duplicated(organ_display, fromLast = TRUE),
          paste0(organ_display, " (", !!sym(organ_col), ")"),
          organ_display
        ),
        order = factor(
          organ_display,
          levels = rev(organ_display[order(!!sym(value_col), decreasing = TRUE)])
        )
      )
  }
  
  # Internal saving function: save plot and data
  save_results <- function(plot_obj, clean_data, 
                           save_plot, plot_path, plot_width, plot_height, plot_dpi, plot_device,
                           save_clean_data, clean_data_path) {
    
    valid_devices <- c("png", "pdf", "svg", "jpeg", "tiff", "bmp", "eps")
    if (!plot_device %in% valid_devices) {
      stop("Invalid plot_device '", plot_device, "'. Valid options: ", paste(valid_devices, collapse = ", "))
    }
    
    # Save plot
    if (save_plot && is.null(plot_obj)) {
      warning("Cannot save plot: no valid plot generated.")
    } else if (save_plot) {
      if (!dir.exists(dirname(plot_path))) {
        dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)
      }
      ggplot2::ggsave(
        filename = plot_path,
        plot = plot_obj,
        width = plot_width,
        height = plot_height,
        dpi = plot_dpi,
        device = plot_device,
        bg = "white"
      )
      message("Plot saved to: ", plot_path)
    }
    
    # Save cleaned data
    if (save_clean_data) {
      if (!dir.exists(dirname(clean_data_path))) {
        dir.create(dirname(clean_data_path), recursive = TRUE, showWarnings = FALSE)
      }
      saveRDS(clean_data, file = clean_data_path)
      message("Cleaned data saved to: ", clean_data_path)
    }
  }
  
  # Main data processing workflow
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("data must be a data frame")
    }
    if (nrow(data) == 0) {
      stop("Input data cannot be empty")
    }
    # Process organ_name_mapping (supports data frames/file paths)
    if (!is.null(organ_name_mapping)) {
      if (is.data.frame(organ_name_mapping) || is.character(organ_name_mapping)) {
        # Call internal helper function to generate mapping vector
        organ_name_mapping <- create_organ_mapping(
          mapping_table = organ_name_mapping,
          output_type = "name_mapping"
        )
      } else if (!is.character(organ_name_mapping) || is.null(names(organ_name_mapping))) {
        stop("organ_name_mapping must be a named vector, data frame, or CSV path")
      }
    }
    
    # Process organ_system_map (supports data frames/file paths)
    if (!identical(organ_system_map, current_default_system) && 
        (is.data.frame(organ_system_map) || is.character(organ_system_map))) {
      organ_system_map <- create_organ_mapping(
        mapping_table = organ_system_map,
        output_type = "system_mapping"
      )
    }
    
    processed_result <- process_organ_data(
      data = data,
      organ_col = organ_col,
      value_col = value_col,
      organ_name_mapping = organ_name_mapping,
      aggregate_method = aggregate_method,
      current_organ_coord = current_organ_coord 
    )
    
    clean_data <- processed_result$clean_data
    missing_organs_from_process <- processed_result$missing_organs
    
    if (!is.null(system) && species == "organelle") {
      warning("system parameter is not applicable for organelle mode - ignoring system filter")
      system <- NULL
    }
    
    # System filtering - DISABLED FOR ORGANELLES
    if (!is.null(system) && species != "organelle") {
      system <- tolower(system)
      system_organs <- unique(organ_system_map$organ[organ_system_map$system == system])
      
      if (length(system_organs) == 0) {
        stop(paste("No organs/organelles found for system:", system))
      }
      
      missing_in_system <- setdiff(system_organs, names(current_organ_coord))
      if (length(missing_in_system) > 0) {
        warning(paste("The following organs in system", system, "have no coordinate data:",
                      paste(missing_in_system, collapse = ", ")))
      }
      
      clean_data <- clean_data[clean_data[[organ_col]] %in% system_organs, ]
      
      if (nrow(clean_data) == 0) {
        warning(paste0("No data available for the specified system: ", system))
      } else {
        if (is.null(title)) {
          title <- paste(stringr::str_to_title(system), "System Visualization")
        }
      }
    }
    
    # Filter invalid organs
    if (!is.null(valid_organs)) {
      valid_organs_std <- standardize_organ_name(valid_organs)
      invalid_organs <- unique(clean_data[[organ_col]][!clean_data[[organ_col]] %in% valid_organs_std])
      
      if (length(invalid_organs) > 0) {
        warning(paste("Found", length(invalid_organs), "invalid organs/organelles:",
                      paste(invalid_organs, collapse = ", ")))
        clean_data <- clean_data[clean_data[[organ_col]] %in% valid_organs_std, ]
        
        if (nrow(clean_data) == 0) {
          stop("No valid organs/organelles remaining after filtering.")
        }
      }
    }
    
    # Sorting
    if (sort_by_value) {
      clean_data <- clean_data[order(clean_data[[value_col]], decreasing = TRUE), ]
    }
    
    data <- clean_data
  }
  
  if (is.null(data)) {
    stop("Input data cannot be empty")
  }
  
  get_component_colors <- function(
    component_low, component_high, component_mid,
    fallback_low, fallback_high, fallback_mid,
    palette, reverse_palette, fillcolor_organ = "plasma", direction = 1) {
    
    # Step 1: Use component-specific colors if provided
    low <- component_low
    high <- component_high
    mid <- component_mid
    
    # Step 2: Fall back to other component colors if primary colors are not set
    if (is.null(low)) low <- fallback_low
    if (is.null(high)) high <- fallback_high
    if (is.null(mid)) mid <- fallback_mid
    
    # Step 3: Generate from palette if colors are still missing
    if (is.null(low) || is.null(high)) {
      
      # Validate palette exists
      if (!palette %in% rownames(RColorBrewer::brewer.pal.info)) {
        stop("Invalid palette: ", palette)
      }
      
      if (fillcolor_organ %in% c('viridis', 'magma', 'inferno', 'plasma', 'cividis')) {
        # Use viridis color scales
        if (!requireNamespace("viridis", quietly = TRUE)) {
          stop("Package 'viridis' is required for viridis color scales")
        }
        viridis_cols <- viridis::viridis(3, option = fillcolor_organ, direction = direction)
        low <- if (is.null(low)) viridis_cols[1] else low
        high <- if (is.null(high)) viridis_cols[3] else high
        mid <- if (is.null(mid)) viridis_cols[2] else mid
      } else {
        # Use RColorBrewer palette
        brew_colors <- RColorBrewer::brewer.pal(n = 3, name = palette)
        if (reverse_palette) brew_colors <- rev(brew_colors)
        low <- if (is.null(low)) brew_colors[1] else low
        high <- if (is.null(high)) brew_colors[3] else high
        mid <- if (is.null(mid)) brew_colors[2] else mid
      }
    }
    
    # Step 4: Validate color formats
    validate_color <- function(color) {
      if (is.null(color)) return(TRUE)
      tryCatch({
        grDevices::col2rgb(color)
        TRUE
      }, error = function(e) FALSE)
    }
    
    if (!validate_color(low) || !validate_color(high) || (!is.null(mid) && !validate_color(mid))) {
      stop("Invalid color format! Use hex codes (e.g., '#FF0000') or named colors (e.g., 'red').")
    }
    
    return(list(low = low, high = high, mid = mid))
  }
  
  # Get colors for heatmap and bar chart separately
  heatmap_colors <- get_component_colors(
    component_low = color_low,
    component_high = color_high,
    component_mid = color_mid,
    # Heatmap falls back to bar chart colors if not set
    fallback_low = organbar_low,    
    fallback_high = organbar_high,  
    fallback_mid = NULL,
    fillcolor_organ = fillcolor_organ,
    direction = direction,
    palette = palette,
    reverse_palette = reverse_palette
  )
  
  bar_colors <- get_component_colors(
    component_low = organbar_low,
    component_high = organbar_high,
    component_mid = NULL,
    # Bar chart falls back to heatmap colors if not set
    fallback_low = color_low,       
    fallback_high = color_high,     
    fallback_mid = color_mid,
    fillcolor_organ = fillcolor_organ,
    direction = direction,
    palette = palette,
    reverse_palette = reverse_palette
  )
  
  # 3. Update other color parameters
  # Keep parameter value
  fillcolor_outline <- fillcolor_outline  
  fillcolor_other <- fillcolor_other      
  
  
  if (!is.null(fillcolor_outline)) {
    # Check if the color format is valid using regular expressions to match common color formats
    if (!grepl("^#([A-Fa-f0-9]{3}|[A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$", fillcolor_outline) &&
        !fillcolor_outline %in% grDevices::colors()) {
      stop("Invalid color format for 'fillcolor_outline'. ",
           "Please provide a valid color name (e.g., 'red') or a hex code (e.g., '#FF0000', '#FFF').")
    }
  }
  
  # Main plotting workflow
  p <- ggplot2::ggplot(data = current_bodycontour, ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_void()
  
  if (outline) {
    p <- p + ggpolypath::geom_polypath(
      data = current_bodycontour,
      ggplot2::aes(group = id),  
      color = "black",   
      alpha = .5,
      linewidth = 0.3,
      fill = fillcolor_outline
    )
  }
  
  if (showall == TRUE) {
    organcoord <- current_organ_coord
    organcoord_all <- data.table::rbindlist(organcoord, fill = TRUE) %>% as.data.frame()
    
    p <- p + ggpolypath::geom_polypath(
      mapping = ggplot2::aes(group = id),
      linewidth = 0.2,
      fill = "#EFEFEF",
      alpha = .2,
      color = "grey",
      data = organcoord_all
    )
  }
  
  # Prepare organ data
  user_organs <- unique(data[[organ_col]])
  
  # For organelles, get all available organelles
  if (species == "organelle") {
    user_organs_data <- get_organ_coordinates(names(current_organ_coord))
  } else {
    user_organs_data <- get_organ_coordinates(user_organs)
  }
  
  # Merge user data
  user_organs_data <- dplyr::left_join(
    user_organs_data,
    data %>% dplyr::rename(organ = all_of(organ_col), value = all_of(value_col)),
    by = "organ"
  )
  
  # Mark in-system organs
  if (species == "organelle") {
    # For organelles, all are considered "in system" and we color based on whether they have data
    user_organs_data$has_data <- !is.na(user_organs_data$value)
    system_organs_data <- user_organs_data %>% dplyr::filter(has_data)
    other_organs_data <- user_organs_data %>% dplyr::filter(!has_data)
  } else {
    # Original logic for human and mouse
    if (!is.null(system)) {
      system_organs <- unique(organ_system_map$organ[organ_system_map$system == system])
      user_organs_data$in_system <- user_organs_data$organ %in% system_organs
    } else {
      user_organs_data$in_system <- TRUE
    }
    
    # Separate in-system and out-of-system organs
    system_organs_data <- user_organs_data %>% dplyr::filter(in_system)
    other_organs_data <- user_organs_data %>% dplyr::filter(!in_system)
  }
  
  # Plot out-of-system organs
  if (nrow(other_organs_data) > 0) {
    p <- p + ggpolypath::geom_polypath(
      data = other_organs_data,
      ggplot2::aes(group = id),
      fill = fillcolor_other,
      color = "black",
      alpha = 0.5,
      linewidth = 0.2
    )
  }
  
  # Plot in-system organs
  if (nrow(system_organs_data) > 0) {
    p <- p + ggpolypath::geom_polypath(
      data = system_organs_data,
      ggplot2::aes(group = id, fill = value),
      color = "black",
      alpha = 0.7,
      linewidth = 0.2
    )
    data_range <- range(system_organs_data$value, na.rm = TRUE)
    
    # Heatmap color scale: use heatmap_colors
    if (!is.null(heatmap_colors$mid)) {
      # 3-color gradient
      p <- p + ggplot2::scale_fill_gradient2(
        low = heatmap_colors$low,
        mid = heatmap_colors$mid,
        high = heatmap_colors$high,
        midpoint = mean(data_range),
        limits = data_range
      )
    } else {
      # 2-color gradient
      p <- p + ggplot2::scale_fill_gradient(
        low = heatmap_colors$low,
        high = heatmap_colors$high,
        limits = data_range
      )
    }
  }
  
  # Add title
  if (is.null(title)) {
    title <- if (species == "organelle") {
      "Organelle Visualization"
    } else if (!is.null(system)) {
      paste(stringr::str_to_title(system), "System Visualization")
    } else {
      "Organ Visualization"
    }
  }
  
  p <- p + ggplot2::ggtitle(title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                      vjust = 2,
                                                      size = 16,
                                                      face = "bold",
                                                      color = "#333333",
                                                      margin = ggplot2::margin(b = 10)))
  
  final_plot <- NULL
  
  # Create organ bar chart and combine
  if (organbar && nrow(system_organs_data) > 0) {
    organ_bar <- create_organ_bar_data(
      system_organs_data %>% dplyr::rename(!!organ_col := organ, !!value_col := value),
      organ_col = organ_col,
      value_col = value_col
    )
    
    # Format value labels
    organ_bar$value_label <- if (all(organ_bar[[value_col]] %% 1 == 0)) {
      as.character(organ_bar[[value_col]])
    } else {
      format(round(organ_bar[[value_col]], organbar_digit), nsmall = organbar_digit)
    }
    
    # Create bar chart
    p_organbar <- if (!is.null(organbar_color)) {
      # Scenario 1: Solid-color bars
      ggplot2::ggplot(organ_bar) +
        ggplot2::aes(x = factor(1), y = order) +
        ggplot2::geom_tile(width = 0.5, fill = organbar_color) +
        ggplot2::geom_text(ggplot2::aes(label = value_label), size = 5, color = "black") +
        ggplot2::scale_y_discrete(position = "right") +
        ggplot2::guides(fill = "none") +
        ggplot2::theme_void() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 15, hjust = 1, margin = ggplot2::margin(r = 10)))
    } else {
      # Scenario 2: Gradient bars
      base_plot <- ggplot2::ggplot(organ_bar) +
        ggplot2::aes(x = factor(1), y = order, fill = !!sym(value_col)) +
        ggplot2::geom_tile(width = 0.5) +
        ggplot2::geom_text(ggplot2::aes(label = value_label), size = 5, color = "black") +
        ggplot2::scale_y_discrete(position = "right") +
        ggplot2::guides(fill = "none") +
        ggplot2::theme_void() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 15, hjust = 1, margin = ggplot2::margin(r = 10)))
      
      # Use 3-color gradient if mid color exists; otherwise use 2-color gradient
      if (!is.null(bar_colors$mid)) {
        base_plot + ggplot2::scale_fill_gradient2(
          low = bar_colors$low,
          mid = bar_colors$mid,
          high = bar_colors$high,
          midpoint = mean(data_range)
        )
      } else {
        base_plot + ggplot2::scale_fill_gradient(
          low = bar_colors$low,
          high = bar_colors$high
        )
      }
    }
    
    # Combine plots
    final_plot <- (p_organbar + p) +
      patchwork::plot_layout(ncol = 2, widths = c(1, 4)) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "right") &
      ggplot2::labs(fill = organbar_title)
  } else {
    final_plot <- p &
      ggplot2::theme(legend.position = "right") &
      ggplot2::labs(fill = organbar_title)
  }
  
  # Warning for no valid organs
  if (nrow(system_organs_data) == 0 && !is.null(system) && species != "organelle") {
    warning(paste("No valid organs/organelles found for", species, "system:", system, "- cannot generate plot."))
    final_plot <- NULL
  } else if (nrow(user_organs_data) == 0) {
    warning(paste("No valid organs/organelles with coordinate data found for", species, "- cannot generate plot."))
    final_plot <- NULL
  }
  
  # Print final plot
  if (!is.null(final_plot)) {
    print(final_plot)
  }
  
  
  if (save_clean_data) {
    if (is.null(clean_data_path)) {
      clean_data_path <- file.path(tempdir(), paste0("clean_data_", sample(1:10000, 1), ".rds"))
    } else {
      if (!dir.exists(dirname(clean_data_path))) {
        dir.create(dirname(clean_data_path), recursive = TRUE, showWarnings = FALSE)
      }
    }
  }
  
  if (save_plot) {
    if (is.null(plot_path)) {
      plot_path <- file.path(tempdir(), paste0("plot_", sample(1:10000, 1), ".png"))
    } else {
      if (!dir.exists(dirname(plot_path))) {
        dir.create(dirname(plot_path), recursive = TRUE, showWarnings = FALSE)
      }
    }
  }
  
  # Call internal saving function
  if (save_plot || save_clean_data) {
    save_results(
      plot_obj = final_plot,
      clean_data = clean_data,
      save_plot = save_plot,
      plot_path = plot_path,
      plot_width = plot_width,
      plot_height = plot_height,
      plot_dpi = plot_dpi,
      plot_device = plot_device,
      save_clean_data = save_clean_data,
      clean_data_path = clean_data_path
    )
  }
  
  # Return results
  invisible(list(
    plot = final_plot,
    clean_data = clean_data,
    system_used = if (species != "organelle" && !is.null(system)) system else "all",
    mapped_organs = unique(clean_data[[organ_col]]),
    missing_organs = missing_organs_from_process, 
    total_value = sum(clean_data[[value_col]], na.rm = TRUE) 
  ))
}

# Human organ systems mapping
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
  stringsAsFactors = FALSE
)

# Mouse organ systems mapping  
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
            "adrenal_gland","thyroid_gland","ovary","testis","pancreas"
  ),
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
  stringsAsFactors = FALSE
)


