 # OrgHeatmap

`OrgHeatmap` is an R package for visualizing numerical data (e.g., gene expression levels, physiological indicators) on human organ diagrams. It supports custom color schemes, organ system filtering, and quantitative bar charts to intuitively display data distribution across anatomical structures.


## Features
- **Visualize data directly on human organ illustrations**
- **Filter visualization by organ systems (circulatory, respiratory, etc.)**
- **Customizable color gradients and outline styles**
- **Integrate bar charts for quantitative comparison**
- **Handle non-standard organ names with mapping functionality**
- **Automatically aggregate duplicate organ entries (mean, sum, count)**


## Installation

### From Local Source
Replace with your actual package file path:
```r
install.packages("OrgHeatmap_0.1.0.tar.gz", repos = NULL, type = "source")
```

### From GitHub 
First install devtools if missing, then install from GitHub:
```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")
devtools::install_github("QiruiShen439/OrgHeatmap")
```
### Required Dependencies
Install all dependent packages to ensure smooth operation:
```r
install.packages(c("ggpolypath", "patchwork", "dplyr", "stringdist", "ggplot2"))
```

## Quick Start

### 1. Load Package and Example Data
```r
library(OrgHeatmap)

# Load built-in example dataset
file_path <- system.file("extdata", "exampledata.Rdata", package = "OrgHeatmap")
load(file_path)

# Inspect data structure (organ names and corresponding values)
head(example_Data3)
```

### 2. Basic Visualization
Create a basic organ visualization with default settings:
```r
# Create basic organ visualization with default settings
result <- OrgHeatmap(data = example_Data3)

# Display the generated plot
print(result$plot)
```

### 3. System-Specific Visualization
Visualize only organs from a specific system (e.g., circulatory system):
```r
# Visualize only organs from the circulatory system
circulatory_plot <- OrgHeatmap(
  data = example_Data3,
  system = "circulatory",
  title = "Circulatory System Data Visualization"
)

# Display the plot
print(circulatory_plot$plot)
```

### 4. Visualization with Bar Chart
Create visualization with accompanying bar chart for quantitative comparison:
```r
bar_plot <- OrgHeatmap(
  data = example_Data3,
  organbar = TRUE,
  organbar_title = "Value Distribution",
  organbar_digit = 1,
  title = "Organ Data Visualization with Bar Chart"
)

# Display the combined plot
print(bar_plot$plot)
```

## Advanced: Custom Color Configuration 
The package provides two flexible ways to configure visualization colors—direct palette usage (for simplicity) and unified color schemes via define_organ_colors (for fine control).

### 1. Using RColorBrewer Palettes Directly (Recommended for Beginners) 
Visualize respiratory system data with a pre-built palette, reversed color order, and custom middle color:
```r
# Visualize respiratory system with "PuBuGn" palette
respiratory_palette_plot <- OrgHeatmap(
  data = example_Data3,
  system = "respiratory",          # Target organ system
  palette = "PuBuGn",              # RColorBrewer palette (blue-purple-green)
  reverse_palette = TRUE,          # Reverse palette: low value = dark green, high value = purple
  color_mid = "#87CEEB",           # Custom middle color (sky blue) for 3-color gradient
  organbar = TRUE,                 # Show bar chart
  organbar_title = "Mean Value",   # Bar chart legend title
  organbar_digit = 2,              # Keep 2 decimal places for bar values
  showall = TRUE,                  # Show grey outlines of all organs (not just respiratory)
  title = "Respiratory System (PuBuGn Palette)",
  save_plot = TRUE, # Enable plot saving
  plot_path = file.path(getwd(), "respiratory_palette_plot.png"), # Custom path for saving the plot
  plot_width = 10, # Plot width (no need to repeat in ggsave)
  plot_height = 8, # Plot height
  plot_dpi = 300 # Plot resolution (function defaults to bg="white", no need to specify separately)
  )

# Display the plot
print(respiratory_palette_plot$plot)

```

### 2. Using `define_organ_colors` for Unified Color Schemes (Advanced) 
For consistent colors across multiple plots (e.g., a series of figures for a paper), use define_organ_colors to generate a reusable color configuration first:
#### Step 1: Generate a Custom Color Scheme
```r
respiratory_colors <- define_organ_colors(
  palette = "PuBuGn",              # Base RColorBrewer palette
  reverse = TRUE,                  # Reverse palette order
  mid = "#87CEEB",                 # Custom middle color (sky blue)
  fillcolor_other = "#E8F4F8",     # Lighter fill for non-target organs (reduces distraction)
  fillcolor_outline = "#F0E6D2",   # Warmer body outline color (matches package default)
  outline_color = "#333333"        # Darker outline (better contrast for small organs)
)
```
#### Step 2: Apply the Color Scheme to Visualization
```r
custom_color_plot <- OrgHeatmap(
  data = example_Data3,
  system = "respiratory",
  # Apply pre-defined colors from the configuration
  color_low = respiratory_colors$fillcolor_low,    # Low value color (from palette)
  color_high = respiratory_colors$fillcolor_high,  # High value color (from palette)
  color_mid = respiratory_colors$fillcolor_mid,    # Middle value color (custom)
  fillcolor_other = respiratory_colors$fillcolor_other,  # Non-target organ color
  fillcolor_outline = respiratory_colors$fillcolor_outline,  # Body outline color
  # Plot styling
  organbar = TRUE,
  organbar_title = "Mean Value",
  organbar_digit = 2,
  showall = TRUE,
  title = "Respiratory System (Custom Color Scheme)",
  # Optional: Save plot and cleaned data
  save_plot = TRUE,
  plot_path = file.path(getwd(), "respiratory_custom_color_plot.png"),
  save_clean_data = TRUE,
  clean_data_path = file.path(getwd(), "respiratory_clean_data.rds")
)

# Display the plot
print(custom_color_plot$plot)

# Inspect the color configuration (for reuse in other plots)
str(respiratory_colors)
```

## Key Advantages of `define_organ_colors`
- **Color Validation**: Automatically checks if colors are valid (e.g., valid hex codes like #87CEEB or named colors like "skyblue").
- **Consistency**: Ensures the same color scheme across heatmaps, bar charts, and organ outlines.
- **Flexibility**: Supports both 2-color (low-high) and 3-color (low-mid-high) gradients.
- **Fallback**: Uses RColorBrewer palette colors if custom colors are not specified (avoids missing values).

## Detailed Documentation
For comprehensive tutorials and parameter explanations:
```r
# Access built-in vignettes
browseVignettes("OrgHeatmap")

# View function documentation
?OrgHeatmap          # Main visualization function
```

## Troubleshooting
- **Missing organs**: Check names against `unique(organ_systems$organ)` for valid identifiers
- **Name mapping issues**: Use `organ_name_mapping` parameter to standardize non-standard names
- **Bar chart not displaying**: Ensure `organbar = TRUE` and valid data exists in `clean_data`
- **Installation errors**: Verify all dependencies are installed and up-to-date

## Maintainer
- Qirui Shen

- Email: shenqr@i.smu.edu.cn



