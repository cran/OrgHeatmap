library(testthat)
library(OrgHeatmap)
library(ggplot2)
library(patchwork)

test_that("Basic Functionality - All Organs Visualization", {
  # 1.Human organ test
  human_data <- data.frame(
    organ = c("heart", "liver", "brain", "kidney"),
    value = c(15, 25, 20, 18)
  )
  human_result <- OrgHeatmap(human_data, save_plot = FALSE)
  
  # Validate output structure
  expect_type(human_result, "list")
  expect_s3_class(human_result$plot, c("gg", "ggplot"))
  expect_s3_class(human_result$clean_data, "data.frame")
  expect_true(nrow(human_result$clean_data) >= 1)
  # Validate total_value (15+25+20+18=78)
  expect_equal(human_result$total_value, 78)
  
  # 2. Organelle test
  organelle_data <- data.frame(
    organ = c("mitochondrion", "nucleus", "endoplasmic_reticulum"),
    value = c(15.2, 8.7, 6.3)
  )
  organelle_result <- OrgHeatmap(organelle_data, species = "organelle", save_plot = FALSE)
  
  # Validate organelle output
  expect_equal(organelle_result$system_used, "all")  # Organelles have no systems
  expect_setequal(organelle_result$mapped_organs, c("mitochondrion", "nucleus", "endoplasmic_reticulum"))
  expect_equal(organelle_result$total_value, 15.2 + 8.7 + 6.3)  # 30.2
})




test_that("Organ System Filtering - Human + Mouse + Organelle (No System for Organelles)", {
  # 1. Human circulatory system
  human_data <- data.frame(
    organ = c("heart", "liver", "brain"),
    value = c(10, 20,  5)
  )
  expect_warning(
    human_circulatory <- OrgHeatmap(human_data, system = "circulatory", save_plot = FALSE),
    "have no coordinate data"
  )
  expect_equal(human_circulatory$system_used, "circulatory")
  expect_true("heart" %in% human_circulatory$clean_data$organ)
  
  # 2. Mouse digestive system
  mouse_data <- data.frame(
    organ = c("stomach", "liver", "heart"),
    value = c(5, 10, 15)
  )
  mouse_digestive <- OrgHeatmap(mouse_data, species = "mouse", system = "digestive", save_plot = FALSE)
  expect_equal(mouse_digestive$system_used, "digestive")
  expect_setequal(mouse_digestive$clean_data$organ, c("stomach", "liver"))  
  
  # 3. Organelle with system parameter (should warn/be ineffective)
  organelle_data <- data.frame(organ = "nucleus", value = 10)
  expect_warning(
    OrgHeatmap(organelle_data, species = "organelle", system = "any_system", save_plot = FALSE),
    "system parameter is not applicable for organelle mode"
  )
})




test_that("Organ Name Mapping - named vector + data.frame + CSV path", {
  test_data <- data.frame(
    organ = c("Heart Organ", "Hepatic Tissue", "Cerebral Cortex", "Adrenal Gland"),
    value = c(12, 18, 9, 6)
  )
  
  # 1. named vector
  vec_mapping <- c(
    "Heart Organ" = "heart",
    "Hepatic Tissue" = "liver",
    "Cerebral Cortex" = "brain",
    "Adrenal Gland" = "adrenal_gland"
  )
  vec_result <- OrgHeatmap(test_data, organ_name_mapping = vec_mapping, save_plot = FALSE)
  expect_setequal(vec_result$mapped_organs, unname(vec_mapping))
  
  # 2. data.frame mapping
  df_mapping <- data.frame(
    original_name = c("Heart Organ", "Hepatic Tissue", "Cerebral Cortex", "Adrenal Gland"),
    standard_name = c("heart", "liver", "brain", "adrenal_gland"),
    stringsAsFactors = FALSE
  )
  df_result <- OrgHeatmap(test_data, organ_name_mapping = df_mapping, save_plot = FALSE)
  expect_setequal(df_result$mapped_organs, df_mapping$standard_name)
  
  # 3. CSV path mapping
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(df_mapping, file = temp_csv, row.names = FALSE)
  csv_result <- OrgHeatmap(test_data, organ_name_mapping = temp_csv, save_plot = FALSE)
  expect_setequal(csv_result$mapped_organs, df_mapping$standard_name)
  unlink(temp_csv)
})




test_that("Fuzzy Matching for Organ Names - Human + Organelle", {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    skip("stringdist package is not installed; skipping fuzzy matching test")
  }
  
  # 1. Human organ fuzzy matching
  human_fuzzy_data <- data.frame(
    organ = c("hearte", "livers", "brai"),
    value = c(10, 20, 15)
  )
  expect_message(
    human_fuzzy_result <- OrgHeatmap(human_fuzzy_data, save_plot = FALSE),
    regexp = "Fuzzy matched.*hearte.*heart|Fuzzy matched.*livers.*liver|Fuzzy matched.*brai.*brain",
    all = TRUE
  )
  expect_setequal(human_fuzzy_result$mapped_organs, c("heart", "liver", "brain"))
  
  # 2. Organelle fuzzy matching
  organelle_fuzzy_data <- data.frame(
    organ = c("mitochondrio", "nucleu", "endoplasmic_reticulu"),
    value = c(5, 8, 3)
  )
  expect_message(
    organelle_fuzzy_result <- OrgHeatmap(organelle_fuzzy_data, species = "organelle", save_plot = FALSE),
    regexp = "Fuzzy matched.*mitochondrio.*mitochondrion|Fuzzy matched.*nucleu.*nucleus|Fuzzy matched.*endoplasmic_reticulu.*endoplasmic_reticulum",
    all = TRUE
  )
  expect_setequal(organelle_fuzzy_result$mapped_organs, c("mitochondrion", "nucleus", "endoplasmic_reticulum"))
})




test_that("Aggregation Methods - Human + Organelle", {
  # 1. Human organ aggregation
  human_agg_data <- data.frame(
    organ = c("heart", "heart", "liver", "brain", "brain", "brain"),
    value = c(10, 20, 15, 5, 10, 15)
  )
  # mean aggregation (15,15,10)
  mean_result <- OrgHeatmap(human_agg_data, aggregate_method = "mean", save_plot = FALSE)
  expect_equal(mean_result$clean_data$value[mean_result$clean_data$organ == "heart"], 15)
  # sum aggregation (30,15,30)
  sum_result <- OrgHeatmap(human_agg_data, aggregate_method = "sum", save_plot = FALSE)
  expect_equal(sum_result$clean_data$value[sum_result$clean_data$organ == "brain"], 30)
  # count aggregation (2,1,3)
  count_result <- OrgHeatmap(human_agg_data, aggregate_method = "count", save_plot = FALSE)
  expect_equal(count_result$clean_data$value[count_result$clean_data$organ == "liver"], 1)
  
  # 2. Organelle aggregation
  organelle_agg_data <- data.frame(
    organ = c("nucleus", "nucleus", "mitochondrion"),
    value = c(5, 10, 15)
  )
  organelle_mean <- OrgHeatmap(organelle_agg_data, species = "organelle", aggregate_method = "mean", save_plot = FALSE)
  expect_equal(organelle_mean$clean_data$value[organelle_mean$clean_data$organ == "nucleus"], 7.5)
})




test_that("Sorting Functionality", {
  test_data <- data.frame(
    organ = c("heart", "liver", "brain"),
    value = c(15, 25, 10)
  )
  
  # Descending sort
  result_sorted <- OrgHeatmap(test_data, sort_by_value = TRUE)
  organs_sorted <- result_sorted$clean_data$organ
  expect_equal(organs_sorted[1], "liver")
})




test_that("Bar Chart Functionality - Gradient + Solid Color (Human + Organelle)", {
  # 1. Human organ gradient bar chart
  human_bar_data <- data.frame(
    organ = c("heart", "liver", "brain"),
    value = c(12.3456, 18.9012, 9.8765)
  )
  expect_silent(
    gradient_bar <- OrgHeatmap(human_bar_data, organbar = TRUE, save_plot = FALSE)
  )
  expect_true(!is.null(gradient_bar$plot))
  
  # 2. Organelle solid color bar chart
  organelle_bar_data <- data.frame(
    organ = c("nucleus", "mitochondrion"),
    value = c(8.7, 15.2)
  )
  expect_silent(
    solid_bar <- OrgHeatmap(
      organelle_bar_data, 
      species = "organelle", 
      organbar = TRUE, 
      organbar_color = "skyblue",  
      save_plot = FALSE
    )
  )
  expect_true(!is.null(solid_bar$plot))
  expect_s3_class(solid_bar$plot, c("gg", "ggplot", "patchwork"))
})




test_that("Color Configuration and Priority Test", {
  test_data <- data.frame(
    organ = c("heart", "liver", "brain"),
    value = c(10, 20, 15)
  )
  
  # --- Scenario 1: Color Priority (organbar_color > other configurations) ---
  expect_silent(
    result_bar_color <- OrgHeatmap(
      test_data,
      organbar = TRUE,
      organbar_color = "skyblue",
      color_low = "red",
      save_plot = FALSE
    )
  )
  expect_true(!is.null(result_bar_color$plot))
  
  # --- Scenario 2: Heatmap Color Priority ---
  expect_silent(
    result_priority <- OrgHeatmap(
      test_data,
      color_low = "#FFE4E1",
      color_high = "#DC143C",
      palette = "YlOrRd",
      fillcolor_organ = "plasma",
      save_plot = FALSE
    )
  )
  expect_true(!is.null(result_priority$plot))
  
  # --- Scenario 3: Palette Functionality ---
  expect_silent(
    result_palette <- OrgHeatmap(
      test_data,
      palette = "PuBuGn",
      reverse_palette = TRUE,
      save_plot = FALSE
    )
  )
  expect_true(!is.null(result_palette$plot))
  
  # --- Scenario 4: 3-Color Gradient ---
  expect_silent(
    result_3color <- OrgHeatmap(
      test_data,
      color_low = "#FF0000",
      color_mid = "#FFFF00",
      color_high = "#0000FF",
      save_plot = FALSE
    )
  )
  expect_true(!is.null(result_3color$plot))
  
  # --- Scenario 5: Fallback Color ---
  expect_silent(
    result_fallback <- OrgHeatmap(
      test_data,
      fillcolor_organ = "plasma",
      save_plot = FALSE
    )
  )
  expect_true(!is.null(result_fallback$plot))
  
  expect_s3_class(result_bar_color$plot, c("gg", "ggplot", "patchwork"))
  expect_s3_class(result_priority$plot, c("gg", "ggplot"))
  expect_s3_class(result_palette$plot, c("gg", "ggplot"))
  expect_s3_class(result_3color$plot, c("gg", "ggplot"))
  expect_s3_class(result_fallback$plot, c("gg", "ggplot"))
})




test_that("Error Handling - Invalid Columns + Invalid Colors + Invalid Species", {
  # 1. Invalid column names
  bad_col_data <- data.frame(organ = "heart", value = 1)
  expect_error(
    OrgHeatmap(bad_col_data, organ_col = "nonexistent"),
    "Specified organ column 'nonexistent' not found in data"
  )
  expect_error(
    OrgHeatmap(bad_col_data, value_col = "nonexistent"),
    "Specified value column 'nonexistent' not found in data"
  )
  
  # 2. Invalid color parameters
  invalid_color_data <- data.frame(organ = "heart", value = 10)
  # Invalid fillcolor_outline
  expect_error(
    OrgHeatmap(invalid_color_data, fillcolor_outline = "#1234"),  # Incomplete hex code
    "Invalid color format for 'fillcolor_outline'"
  )
  # Invalid color_low
  expect_error(
    OrgHeatmap(invalid_color_data, color_low = "invalid_color"),  # Non-existent color name
    "Invalid color format"
  )
  # Invalid organbar_high
  expect_error(
    OrgHeatmap(invalid_color_data, organbar_high = "#ABCDEF123"),  # Overlong hex code
    "Invalid color format"
  )
  
  # 3. Invalid species
  expect_error(
    OrgHeatmap(invalid_color_data, species = "invalid_species"),
    "should be one of|must be one of" 
  )
})



test_that("Missing Organ Handling - Human + Organelle", {
  # 1. Missing human organs
  human_missing_data <- data.frame(
    organ = c("heart", "unknown_organ1", "liver", "unknown_organ2"),
    value = c(10, 5, 15, 8)
  )
  
  human_missing_result <- OrgHeatmap(human_missing_data, save_plot = FALSE)
  
  expect_true(all(c("heart", "liver") %in% human_missing_result$clean_data$organ))
  expect_true(all(c("unknown_organ1", "unknown_organ2") %in% human_missing_result$missing_organs))
  
  # 2. Missing organelles
  organelle_missing_data <- data.frame(
    organ = c("nucleus", "unknown_organelle", "mitochondrion"),
    value = c(8.7, 5.0, 15.2)
  )
  
  organelle_missing_result <- OrgHeatmap(organelle_missing_data, species = "organelle", save_plot = FALSE)
  
  expect_equal(nrow(organelle_missing_result$clean_data), 2)
  expect_true("unknown_organelle" %in% organelle_missing_result$missing_organs)
})



test_that("Display Options - showall + outline (Human + Organelle)", {
  test_data <- data.frame(organ = c("heart", "liver"), value = c(10, 15))
  
  # 1. Human showall=TRUE
  expect_silent(
    human_showall <- OrgHeatmap(test_data, showall = TRUE, save_plot = FALSE)
  )
  expect_true(!is.null(human_showall$plot))
  
  # 2. Organelle showall=TRUE
  organelle_showall_data <- data.frame(organ = "nucleus", value = 8.7)
  expect_silent(
    organelle_showall <- OrgHeatmap(
      organelle_showall_data,
      species = "organelle",
      showall = TRUE,
      save_plot = FALSE
    )
  )
  expect_true(!is.null(organelle_showall$plot))
  
  # 3. outline=FALSE 
  expect_silent(
    no_outline <- OrgHeatmap(test_data, outline = FALSE, save_plot = FALSE)
  )
  expect_true(!is.null(no_outline$plot))
  
  expect_s3_class(human_showall$plot, c("gg", "ggplot"))
  expect_s3_class(organelle_showall$plot, c("gg", "ggplot"))
  expect_s3_class(no_outline$plot, c("gg", "ggplot"))
})



test_that("Mouse Species Test - Contour Closing + Outlier Handling", { 
  mouse_test_data <- data.frame(
    organ = c("heart", "liver"),
    value = c(10, 15)
  )

  expect_silent(
    mouse_result <- suppressWarnings(OrgHeatmap(mouse_test_data, species = "mouse", save_plot = FALSE))
  )
  
  # Validate mouse contour layer
  expect_true(!is.null(mouse_result$plot))
  expect_s3_class(mouse_result$plot, c("gg", "ggplot"))
})




test_that("Color Gradient Direction Parameter (direction)", {
  test_data <- data.frame(organ = c("heart", "liver"), value = c(10, 20))
  
  # Test default direction (direction = 1)
  expect_silent(
    result_default <- OrgHeatmap(
      test_data, 
      color_low = "blue", 
      color_high = "red", 
      save_plot = FALSE
    )
  )
  expect_true(!is.null(result_default$plot))
  
  # Test reversed direction (direction = -1)
  expect_silent(
    result_reversed <- OrgHeatmap(
      test_data, 
      color_low = "blue", 
      color_high = "red", 
      direction = -1, 
      save_plot = FALSE
    )
  )
  expect_true(!is.null(result_reversed$plot))
  
  expect_s3_class(result_default$plot, c("gg", "ggplot"))
  expect_s3_class(result_reversed$plot, c("gg", "ggplot"))
})




test_that("Custom Organ System Mapping (dataframe format)", {
  custom_system_map <- data.frame(
    organ = c("heart", "liver", "custom_organ"),
    system = c("custom_system", "custom_system", "custom_system")
  )
  
  test_data <- data.frame(
    organ = c("heart", "liver", "custom_organ"),
    value = c(10, 20, 15)
  )
  
  expect_warning(
    result <- OrgHeatmap(
      test_data,
      system = "custom_system",
      organ_system_map = custom_system_map,
      save_plot = FALSE
    ),
    "have no coordinate data"
  )
  
  # Validate custom system
  expect_equal(result$system_used, "custom_system")
  expect_true(all(result$clean_data$organ %in% c("heart", "liver")))
  expect_true("custom_organ" %in% result$missing_organs)
})



test_that("Custom Organ System Mapping (CSV file path)", {
  # 1. Create temporary CSV file (custom system mapping)
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      organ = c("heart", "liver", "custom_organ"),
      system = c("my_system", "my_system", "my_system"),
      stringsAsFactors = FALSE
    ),
    file = temp_csv,
    row.names = FALSE
  )
  
  # 2. Test mapping via CSV path
  test_data <- data.frame(
    organ = c("heart", "liver", "custom_organ"),
    value = c(10, 20, 15)
  )
  
  expect_warning(
    result_csv_map <- OrgHeatmap(
      test_data,
      system = "my_system",
      organ_system_map = temp_csv
    ),
    "have no coordinate data"
  )
  
  # Validate system filtering result
  expect_equal(result_csv_map$system_used, "my_system")
  expect_true(all(c("heart", "liver") %in% result_csv_map$clean_data$organ))
  expect_true("custom_organ" %in% result_csv_map$missing_organs)
  
  # Clean up temporary file
  unlink(temp_csv)
})



test_that("Empty Input and Boundary Value Tests", {
  # Empty data frame
  empty_df <- data.frame()
  expect_error(
    OrgHeatmap(empty_df),
    "cannot be empty|not found in data"
  )
  
  empty_with_cols <- data.frame(organ = character(), value = numeric())
  expect_error(
    OrgHeatmap(empty_with_cols),
    "cannot be empty"
  )
  
  # Single organ test
  expect_silent(OrgHeatmap(data.frame(organ = "heart", value = 10), save_plot = FALSE))
  
  # Zero and negative values
  expect_silent(OrgHeatmap(data.frame(organ = "heart", value = 0), save_plot = FALSE))
  expect_silent(OrgHeatmap(data.frame(organ = "heart", value = -5), save_plot = FALSE))
})



test_that("Custom Column Name Test", {
  test_data <- data.frame(
    custom_organ_col = c("heart", "liver"),
    custom_value_col = c(15, 25)
  )
  
  result <- OrgHeatmap(
    test_data,
    organ_col = "custom_organ_col",
    value_col = "custom_value_col"
  )
  
  # Validate custom column names are retained
  expect_true("custom_organ_col" %in% colnames(result$clean_data), 
              info = "Custom organ column name not retained in clean_data")
  expect_true("custom_value_col" %in% colnames(result$clean_data), 
              info = "Custom value column name not retained in clean_data")
})




test_that("Saving Functionality - Multiple Formats + Nested Directories + Organelle", {
  test_data <- data.frame(organ = c("heart", "liver"), value = c(10, 15))
  temp_dir <- tempdir()
  
  # 1. Save human organs (PNG + RDS)
  human_plot_path <- file.path(tempdir(), "human_save_plot.png")
  human_data_path <- file.path(tempdir(), "human_save_data.rds")
  
  human_save_result <- OrgHeatmap(
    test_data,
    save_plot = TRUE,
    plot_path = human_plot_path,
    save_clean_data = TRUE,
    clean_data_path = human_data_path,
    plot_width = 8,
    plot_height = 6
  )
  expect_true(file.exists(human_plot_path))  
  expect_true(file.exists(human_data_path))
  saved_human_data <- readRDS(human_data_path)
  expect_equal(saved_human_data, human_save_result$clean_data)
  
  
  
  # 2. Save organelles (PDF + SVG)
  organelle_pdf_path <- file.path(tempdir(), "organelle_save.pdf")  
  organelle_svg_path <- file.path(tempdir(), "organelle_save.svg")  
  organelle_data <- data.frame(organ = "nucleus", value = 8.7)
  
  # PDF save
  OrgHeatmap(
    organelle_data,
    species = "organelle",
    save_plot = TRUE,
    plot_path = organelle_pdf_path, 
    plot_device = "pdf"
  )
  expect_true(file.exists(organelle_pdf_path))
  
  # SVG save
  OrgHeatmap(
    organelle_data,
    species = "organelle",
    save_plot = TRUE,
    plot_path = organelle_svg_path, 
    plot_device = "svg"
  )
  expect_true(file.exists(organelle_svg_path))
  
  # 3. Save to nested directory (auto-create)
  nested_plot_path <- file.path(tempdir(), "nested", "subdir", "organ_plot.png") 
  nested_data_path <- file.path(tempdir(), "nested", "subdir", "organ_data.rds")
  
  OrgHeatmap(
    test_data,
    save_plot = TRUE,
    plot_path = nested_plot_path,
    save_clean_data = TRUE,
    clean_data_path = nested_data_path
  )
  expect_true(file.exists(nested_plot_path))
  
  # Clean up
  unlink(c(human_plot_path, human_data_path, organelle_pdf_path, organelle_svg_path))
  unlink(file.path(tempdir(), "nested"), recursive = TRUE)
})

