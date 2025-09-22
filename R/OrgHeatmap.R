#' Visualization Tool for Human Organ Data
#' 
#' This tool visualizes numerical data (such as gene expression) on a human organ map.
#' It supports custom color schemes, organ system filtering, and bar charts for quantitative comparison.
#' 
#' @param data Data frame with at least two columns: organ name and corresponding value
#' @param system Optional character vector specifying organ system to display
#' @param valid_organs Optional character vector of valid organ names for filtering
#' @param sort_by_value Logical, default TRUE, sorts by value descending
#' @param title Optional character vector for plot title
#' @param showall Logical, default FALSE. If TRUE, shows all organ outlines (grey) with light grey fill (#EFEFEF) for non-target organs (to provide anatomical context).
#' @param outline Logical, default TRUE, draws human outline
#' @param palette Character, name of RColorBrewer palette (e.g., "YlOrRd", "PuBuGn") for unified color scheme (applies to both organ heatmap and bar chart if no custom colors are specified). 
#'   Ignored if `color_low`/`color_high` (for heatmap) or `organbar_low`/`organbar_high` (for bar chart) are specified. Default: "YlOrRd" (suitable for highlighting high values).
#' @param color_low Character, custom color for the **minimum value** of the organ heatmap (and bar chart if `organbar_low` is not specified). Overrides `palette` but is overridden by `organbar_low` (highest priority). Default: NULL.
#' @param color_high Character, custom color for the **maximum value** of the organ heatmap (and bar chart if `organbar_high` is not specified). Overrides `palette` but is overridden by `organbar_high` (highest priority). Default: NULL.
#' @param color_mid Character, optional color for the **middle value** of the organ heatmap (for 3-color gradients). Default: NULL.
#' @param reverse_palette Logical, whether to reverse the color order of `palette`. Default: FALSE (low=light, high=dark).
#' @param fillcolor_outline Character, default "#F5D5B8", fill color for outline
#' @param fillcolor_organ Character, fallback color scheme for organs (supports viridis options: "viridis", "plasma", "magma", etc.). Only used if no `palette`, `color_low/color_high`, or `organbar_low/organbar_high` are specified. Default: "plasma".
#' @param fillcolor_other Character, default "#D3D3D3", fill color for non-target organs
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
#' @param organ_system_map Data frame, default organ_systems. Can be a custom data frame or CSV path 
#'   (must contain 'organ' and 'system' columns), processed by internal `create_organ_mapping()`.
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
#'   plot_path = file.path(getwd(), "all_system.png"),  # Save to current directory
#'   plot_width = 10,
#'   plot_height = 8,
#'   save_clean_data = TRUE,  # Enable cleaned data saving
#'   clean_data_path = file.path(getwd(), "all_system_clean_data.rds")
#' )
#' print(result_all$plot)  # Print the plot to the console
#' 
#' # 2. Plot circulatory system organs and save results
#' result_circulatory <- OrgHeatmap(
#'   example_Data3, 
#'   system = "circulatory",
#'   organbar = TRUE,
#'   save_plot = TRUE,
#'   plot_path = file.path(getwd(), "circulatory_system.png"),
#'   plot_width = 10,
#'   plot_height = 8,
#'   plot_device = "png",  # Specify plot format
#'   save_clean_data = TRUE,
#'   clean_data_path = file.path(getwd(), "circulatory_clean_data.rds")
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
#'plot_path = file.path(getwd(), "respiratory_palette.png")
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
#'   organ_systems,  # Package's built-in organ system mapping
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
#'   plot_path = file.path(getwd(), "tp53_expression_metastatic_prostate.png"),
#'   plot_width = 14,
#'   plot_height = 10,
#'   plot_dpi = 300,
#'   save_clean_data = TRUE,  # Save cleaned data
#'   clean_data_path = file.path(getwd(), "tp53_clean_data.rds")
#' )
#' 
#' # Print the plot
#' print(tp53_plot$plot)
#' }
#'
#' @details 
#' The function uses \code{define_organ_colors()} (an internal helper function) to generate unified color schemes:
#' 1. If `organbar_low` and `organbar_high` are specified by the user, they will be used directly (highest priority);
#' 2. If not, colors are generated from the `palette` (RColorBrewer) with optional reversal (`reverse_palette`);
#' 3. Custom middle color (`color_mid`) is supported for 3-color gradients (applied to both heatmap and bar chart).
#' For valid `palette` names, see `RColorBrewer::brewer.pal.info`.
#' 
#' ## Organ Mapping Logic
#' - For \code{organ_name_mapping}: Accepts a named vector, data frame, or CSV path. 
#'   Internal helper `create_organ_mapping()` standardizes names (lowercase, underscores for spaces).
#' - For \code{organ_system_map}: Custom tables (data frame/CSV) are processed to align with 
#'   built-in `organ_systems` format via `create_organ_mapping()`.
#' 
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
#' @export
   OrgHeatmap <- function(data,
                          system = NULL,
                          valid_organs = NULL,
                          sort_by_value = TRUE,
                          title = NULL,
                          showall = FALSE,
                          outline = TRUE,
                          palette = "YlOrRd",         # Palette name (RColorBrewer)
                          color_high = NULL,          # Custom color for heatmap maximum(overrides palette)                        
                          color_low = NULL,           # Custom color for heatmap minimum (overrides palette)
                          color_mid = NULL,           # Custom color for heatmap middle (for 3-color gradients)
                          reverse_palette = FALSE,    # Whether to reverse the color order of `palette` (default: FALSE)
                          fillcolor_outline = "#F5D5B8",
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
                          plot_path = file.path(getwd(), "organ_plot.png"),        
                          plot_width = 10,
                          plot_height = 8,
                          plot_dpi = 300,
                          plot_device = "png",
                          organ_system_map = organ_systems,
                          organ_name_mapping = NULL,
                          aggregate_method = "mean",
                          organ_col = "organ",
                          value_col = "value"
) {  
  
  .dummy <- ggplot2::GeomPolygon
  
  if (nrow(data) == 0) {
    stop("Input data cannot be empty (nrow(data) == 0).")
  }
  
  # Helper function: standardize organ name format (space to underscore, lowercase)
  standardize_organ_name <- function(name) {
    tolower(gsub("\\s+", "_", name))
  }
  
  
  # Preprocess organ system mapping (standardize names)
  preprocess_organ_system_map <- function(map) {
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
      mutate(
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
  human_organ_coord <- standardize_coord_names(human_organ_coord)
  
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
    stop(paste("Specified organ column '", organ_col, "' not found in data"))
  }
  if (!value_col %in% colnames(data)) {
    stop(paste("Specified value column '", value_col, "' not found in data"))
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
        sapply(mapping_df[[standard_col]], standardize_organ_name),  # 用 standardize_organ_name 而非 standardize
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
  
  
  
  # Data preprocessing function (standardization, mapping, aggregation)
  process_organ_data <- function(data, organ_col, value_col, organ_name_mapping, aggregate_method) {
  # 1. Input validation : Prevent invalid input from entering the pipeline
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame")
    }
    if (nrow(data) == 0) {
      stop("'data' cannot be empty (nrow(data) == 0)")
    }
    if (!organ_col %in% colnames(data)) {
      stop("organ_col '", organ_col, "' not found in data")
    }
    if (!value_col %in% colnames(data)) {
      stop("value_col '", value_col, "' not found in data")
    }
    if (!aggregate_method %in% c("count", "sum", "mean")) {
      stop("aggregate_method must be one of 'count', 'sum', 'mean'")
    }
    
    fuzzy_matches <- character(0)
    
    # 2. Data processing pipeline 
    data %>%
      # Rename columns and keep required columns
      rename(
        temp_organ = all_of(organ_col),
        temp_value = all_of(value_col)
      ) %>%
      select(temp_organ, temp_value) %>%  
      
    # Ensure conditional branches always return data frame (avoid NULL)
    {
      # Only perform mapping when organ_name_mapping is a character vector, otherwise return original data
      if (!is.null(organ_name_mapping) && is.character(organ_name_mapping)) {
        mutate(., 
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
      mutate(temp_organ = standardize_organ_name(temp_organ)) %>%

      # Fuzzy matching 
      mutate(temp_organ = purrr::map_chr(temp_organ, function(x) {
        all_organs <- unique(organ_system_map$organ)
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
      filter(!is.na(temp_organ), !is.na(temp_value)) %>%
      mutate(temp_value = as.numeric(temp_value)) %>%
      filter(!is.na(temp_value)) %>%  # Filter values that failed numeric conversion
      
      # Aggregate data (ensure at least one row remains after grouping)
      {
        if (nrow(.) == 0) {
          warning("No valid data remaining after filtering. Returning empty data frame.")
          .  # Return empty data frame instead of breaking
        } else {
          group_by(., temp_organ) %>%
            summarise(
              temp_value = switch(
                aggregate_method,
                "count" = n(),
                "sum" = sum(temp_value, na.rm = TRUE),
                "mean" = mean(temp_value, na.rm = TRUE)
              ),
              .groups = "drop"
            )
        }
      } %>%
      
      # Restore user-specified column names
      rename(
        !!organ_col := temp_organ,
        !!value_col := temp_value
      )
  }

  
  define_organ_colors <- function(
    palette = "YlOrRd",
    low = NULL,
    high = NULL,
    mid = NULL,
    fillcolor_other = "#E0E0E0",
    fillcolor_outline = "#F5D5B8",
    outline_color = "black",
    reverse = FALSE
  ) {
    # 1. Validate RColorBrewer palette validity
    if (!is.null(palette) && !palette %in% rownames(RColorBrewer::brewer.pal.info)) {
      valid_palettes <- paste(rownames(RColorBrewer::brewer.pal.info), collapse = ", ")
      stop(paste0(
        "Invalid 'palette' name: '", palette, "'.\n",
        "Valid RColorBrewer palettes: ", valid_palettes
      ))
    }
    
    # 2. Validate color format (avoid invalid colors)
    is_valid_color <- function(color) {
      if (is.null(color)) return(TRUE)
      tryCatch({
        grDevices::col2rgb(color)
        TRUE
      }, error = function(e) FALSE)
    }
    color_params <- list(low=low, high=high, mid=mid, fillcolor_other=fillcolor_other, fillcolor_outline=fillcolor_outline, outline_color=outline_color)
    invalid_colors <- Filter(function(x) !is_valid_color(color_params[[x]]), names(color_params))
    if (length(invalid_colors) > 0) {
      stop(paste0(
        "Invalid color format for: ", paste(invalid_colors, collapse = ", "), "\n",
        "Use hex codes (e.g., #FF0000) or named colors (e.g., 'red')."
      ))
    }
    
    # 3. Generate gradient colors (custom colors first, then palette)
    if (!is.null(low) && !is.null(high)) {
      fillcolor_low <- low
      fillcolor_high <- high
    } else {
      brew_colors <- RColorBrewer::brewer.pal(n = 3, name = palette)
      if (reverse) brew_colors <- rev(brew_colors)
      fillcolor_low <- if (is.null(low)) brew_colors[1] else low
      fillcolor_high <- if (is.null(high)) brew_colors[3] else high
      mid <- if (is.null(mid)) brew_colors[2] else mid
    }
    
    # 4. Return standardized color list
    list(
      fillcolor_low = fillcolor_low,
      fillcolor_high = fillcolor_high,
      fillcolor_mid = mid,
      fillcolor_other = fillcolor_other,
      fillcolor_outline = fillcolor_outline,
      outline_color = outline_color
    )
  }
  
  # Get organ coordinates function
  get_organ_coordinates <- function(organs) {
    valid_organs <- list()
    missing_organs <- character()
    standardized_organs <- standardize_organ_name(organs)
    
    for (org in unique(standardized_organs)) {
      if (org %in% names(human_organ_coord)) {
        df <- human_organ_coord[[org]]
        df$organ <- org
        valid_organs[[org]] <- df
      } else {
        missing_organs <- c(missing_organs, org)
      }
    }
    
    if (length(missing_organs) > 0) {
      warning(paste(
        "The following organs have no coordinate data:",
        paste(unique(missing_organs), collapse = ", ")
      ))
    }
    
    dplyr::bind_rows(valid_organs)
  }
  
  # Create bar chart data function
  create_organ_bar_data <- function(system_organs_data, organ_col, value_col) {
    system_organs_data %>% 
      distinct(!!sym(organ_col), !!sym(value_col)) %>% 
      mutate(
        organ_display = str_replace_all(!!sym(organ_col), "_", " ") %>% 
          str_to_title(),
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
    
    #Save plot
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
      stop("data must be a data frame.")
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
    if (!identical(organ_system_map, organ_systems) && (is.data.frame(organ_system_map) || is.character(organ_system_map))) {
      organ_system_map <- create_organ_mapping(
        mapping_table = organ_system_map,
        output_type = "system_mapping"
      )
    }
    
    clean_data <- process_organ_data(
      data = data,
      organ_col = organ_col,
      value_col = value_col,
      organ_name_mapping = organ_name_mapping,
      aggregate_method = aggregate_method
    )
    
    # System filtering
    if (!is.null(system)) {
      system <- tolower(system)
      system_organs <- unique(organ_system_map$organ[organ_system_map$system == system])
      if (length(system_organs) == 0) {
        stop(paste("No organs found for system:", system))
      }
      if (is.null(title)) {
        title <- paste(str_to_title(system), "System Visualization")
      }
    }
    
    # Filter invalid organs
    if (!is.null(valid_organs)) {
      valid_organs_std <- standardize_organ_name(valid_organs)
      invalid_organs <- unique(clean_data[[organ_col]][!clean_data[[organ_col]] %in% valid_organs_std])
      
      if (length(invalid_organs) > 0) {
        warning(paste("Found", length(invalid_organs), "invalid organs:",
                      paste(invalid_organs, collapse = ", ")))
        clean_data <- clean_data[clean_data[[organ_col]] %in% valid_organs_std, ]
        
        if (nrow(clean_data) == 0) {
          stop("No valid organs remaining after filtering.")
        }
      }
    }
    
    # Sorting
    if (sort_by_value) {
      clean_data <- clean_data[order(clean_data[[value_col]], decreasing = TRUE), ]
    }
    
  
    
    data<-clean_data
  }
    
  
  if (is.null(data)) {
    stop("data is null.")
  }
  
 # Use palette-generated colors if organbar_low/high are not specified by user
 color_config <- define_organ_colors(
   palette = palette,
   low = if (is.null(organbar_low)) color_low else organbar_low,  # Prioritize user-specified bar colors
   high = if (is.null(organbar_high)) color_high else organbar_high,
   mid = color_mid,
   fillcolor_other = fillcolor_other,
   fillcolor_outline = fillcolor_outline,
   outline_color = "black",  # Can be changed to user-defined parameter
   reverse = reverse_palette
)

 # Update color parameters (override original parameters with palette-generated config)
 fillcolor_outline <- color_config$fillcolor_outline
 fillcolor_other <- color_config$fillcolor_other
 organbar_low <- color_config$fillcolor_low
 organbar_high <- color_config$fillcolor_high
 organbar_mid <- color_config$fillcolor_mid  #middle color for 3-color gradients
 
 if (!is.null(fillcolor_outline)) {
   # Check if the color format is valid using regular expressions to match common color formats
   if (!grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$", fillcolor_outline) &&
       !fillcolor_outline %in% grDevices::colors()) {
     stop("Invalid color format for 'fillcolor_outline'. ",
          "Please provide a valid color name (e.g., 'red') or a hex code (e.g., '#FF0000').")
   }
 }

  # Main plotting workflow
  p <- ggplot(data = human_bodycontour, aes(x = x, y = y)) +
    theme_void()
  
  if (outline) {
    p <- p + ggpolypath::geom_polypath(data = human_bodycontour,
                           aes(x = x, y = y),
                           color = color_config$outline_color,  
                           alpha = .5,
                           linewidth = 0.3,
                           fill = fillcolor_outline)
  }
  
  if (showall == TRUE) {
    organcoord <- human_organ_coord
    organcoord_all <- data.table::rbindlist(organcoord, fill = TRUE) %>% as.data.frame()
    
    p <- p + ggpolypath::geom_polypath(mapping = aes(group = id),
                           linewidth = 0.2,
                           fill = "#EFEFEF",
                           alpha = .2,
                           color = "grey",
                           data = organcoord_all)
  }
  
  # Prepare organ data
  user_organs <- unique(data[[organ_col]])
  user_organs_data <- get_organ_coordinates(user_organs)
  
  # Merge user data
  user_organs_data <- dplyr::left_join(
    user_organs_data,
    data %>% rename(organ = all_of(organ_col), value = all_of(value_col)),
    by = "organ"
  )
  
  # Mark in-system organs
  if (!is.null(system)) {
    system_organs <- unique(organ_system_map$organ[organ_system_map$system == system])
    user_organs_data$in_system <- user_organs_data$organ %in% system_organs
  } else {
    user_organs_data$in_system <- TRUE
  }
  
  # Separate in-system and out-of-system organs
  system_organs_data <- user_organs_data %>% dplyr::filter(in_system)
  other_organs_data <- user_organs_data %>% dplyr::filter(!in_system)
  
  # Plot out-of-system organs
  if (nrow(other_organs_data) > 0) {
    p <- p + ggpolypath::geom_polypath(
      data = other_organs_data,
      aes(group = id),
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
      aes(group = id, fill = value),
      color = "black",
      alpha = 0.7,
      linewidth = 0.2
    )
    data_range <- range(system_organs_data$value, na.rm = TRUE)
  }
  
  # Add title
  if (is.null(title)) {
    title <- if (!is.null(system)) {
      paste(str_to_title(system), "System Visualization")
    } else {
      "Organ Visualization"
    }
  }
  
  p <- p + ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5,
                                    vjust = 2,
                                    size = 16,
                                    face = "bold",
                                    color = "#333333",
                                    margin = margin(b = 10)))
  
  final_plot <- NULL
  
  # Create organ bar chart and combine
  if (organbar && nrow(system_organs_data) > 0) {
    organ_bar <- create_organ_bar_data(
      system_organs_data %>% rename(!!organ_col := organ, !!value_col := value),
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
      ggplot(organ_bar) +
        aes(x = factor(1), y = order) +
        geom_tile(width = 0.5, fill = organbar_color) +
        geom_text(aes(label = value_label), size = 5, color = "black") +
        scale_y_discrete(position = "right") +
        theme_void() +
        theme(axis.text.y = element_text(size = 15, hjust = 1, margin = margin(r = 10)))
    } else if (!is.null(organbar_low) && !is.null(organbar_high)) {
      # Scenario 2: 2-color / 3-color gradient bars
      base_plot <- ggplot(organ_bar) +
        aes(x = factor(1), y = order, fill = !!sym(value_col)) +
        geom_tile(width = 0.5) +
        geom_text(aes(label = value_label), size = 5, color = "black") +
        scale_y_discrete(position = "right") +
        guides(fill = "none") +
        theme_void() +
        theme(axis.text.y = element_text(size = 15, hjust = 1, margin = margin(r = 10)))
      
      # Key: Use 3-color gradient if mid color exists; otherwise use 2-color gradient
      if (!is.null(organbar_mid)) {
        base_plot + scale_fill_gradient2(
          low = organbar_low,
          mid = organbar_mid,
          high = organbar_high,
          midpoint = mean(data_range)
        )
      } else {
        base_plot + scale_fill_gradient(low = organbar_low, high = organbar_high)
      }
    } else {
      # Scenario 3: Use default palette colors
      ggplot(organ_bar) +
        aes(x = factor(1), y = order, fill = !!sym(value_col)) +
        geom_tile(width = 0.5) +
        geom_text(aes(label = value_label), size = 5, color = "black") +
        scale_y_discrete(position = "right") +
        guides(fill = "none") +
        theme_void() +
        theme(axis.text.y = element_text(size = 15, hjust = 1, margin = margin(r = 10)))
    }
    
    # Combine plots
    final_plot <- (p_organbar + p) +
      patchwork::plot_layout(ncol = 2, widths = c(1, 4))
    
    # Unified color scale
    if (is.null(organbar_low) || is.null(organbar_high)) {
      if (fillcolor_organ %in% c('viridis', 'magma', 'inferno', 'plasma', 'cividis')) {
        final_plot <- final_plot &
          scale_fill_viridis_c(option = fillcolor_organ,
                               limits = data_range,
                               direction = direction,
                               low = organbar_low,  # Use palette-generated low color
                               high = organbar_high # Use palette-generated high color
          )
      } else {
        final_plot <- final_plot &
          scale_fill_distiller(palette = fillcolor_organ, 
                               limits = data_range, 
                               direction = direction,
                               low = organbar_low,  
                               high = organbar_high )
      }
    }
    
    final_plot <- final_plot +
      patchwork::plot_layout(guides = "collect") &
      theme(legend.position = "right") &
      labs(fill = organbar_title)
  } else {
    if (exists("data_range")) {
      if (fillcolor_organ %in% c('viridis', 'magma', 'inferno', 'plasma', 'cividis')) {
        p <- p + scale_fill_viridis_c(option = fillcolor_organ, limits = data_range, direction = direction,low = organbar_low, high = organbar_high)
      } else {
        p <- p + scale_fill_distiller(palette = fillcolor_organ, limits = data_range, direction = direction,low = organbar_low, high = organbar_high)
      }
      final_plot <- p
    }
  }
  
  # Print final plot
  if (!is.null(final_plot)) {
    print(final_plot)
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
    system_used = if (!is.null(system)) system else "all",
    mapped_organs = user_organs,
    missing_organs = setdiff(unique(data[[organ_col]]), names(human_organ_coord)),
    total_value = sum(clean_data[[value_col]])
  ))
}

# Built-in organ system mapping data frame
organ_systems <- data.frame(
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


