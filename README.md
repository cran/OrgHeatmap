 # OrgHeatmap

`OrgHeatmap` is an R package for visualizing numerical data (e.g., gene expression levels, physiological indicators) on human, mouse, and organelle diagrams. It supports custom color schemes, organ system filtering, and quantitative bar charts to intuitively display data distribution across anatomical structures.


## Features
- **Multi-species Support: Visualize data on human, mouse, or organelle diagrams**
- **Flexible Color Schemes: Unified color configuration for heatmaps and bar charts**
- **Organ System Filtering: Focus on specific anatomical systems (circulatory, respiratory, etc.)**
- **Quantitative Comparison: Integrated bar charts for value comparison**
- **Name Standardization: Handle non-standard organ names with mapping functionality**
- **Data Aggregation: Automatic handling of duplicate organ entries (mean, sum, count)**
- **High-Quality Output: Save plots and cleaned data in multiple formats**


## Installation

### From Local Source
```r
install.packages("OrgHeatmap_0.3.1.tar.gz", repos = NULL, type = "source")
```

### From GitHub 
```r
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")
devtools::install_github("QiruiShen439/OrgHeatmap")
```

### Required Dependencies
The package automatically checks for and installs required dependencies:
```r
# Core dependencies (automatically checked)
install.packages(c("sf", "dplyr", "stringdist", "ggpolypath", "patchwork", 
                   "viridis", "data.table", "stringr", "RColorBrewer", 
                   "ggplot2", "purrr"))
```

## Quick Start

### 1. Load Package and Example Data
```r
library(OrgHeatmap)

# Load built-in example dataset
data_path <- system.file("extdata", "exampledata.Rdata", package = "OrgHeatmap")
load(data_path)

# Inspect data structure
head(example_Data3)
```

### 2. Basic Human Organ Visualization
```r
# Create basic organ visualization with default settings
result <- OrgHeatmap(data = example_Data3)
print(result$plot)
```


### 3. Mouse Organ Visualization
```r
# Visualize mouse digestive system
mouse_result <- OrgHeatmap(
  data = example_Data1,
  species = "mouse",
  system = "digestive",
  palette = "PuBu",
  title = "Mouse Digestive System"
)
print(mouse_result$plot)
```


### 4. Organelle Visualization
```r
# Create organelle data
organelle_data <- data.frame(
  organ = c("mitochondrion", "nucleus", "endoplasmic_reticulum", "cell_membrane"),
  value = c(15.2, 8.7, 6.3, 6.8)
)

# Visualize organelles
organelle_result <- OrgHeatmap(
  data = organelle_data,
  species = "organelle",
  title = "Organelle Expression"
)
print(organelle_result$plot)
```



## Advanced Usage

### System-Specific Visualization
```r
# Focus on specific organ systems
circulatory_plot <- OrgHeatmap(
  data = example_Data3,
  system = "circulatory",
  title = "Circulatory System Data",
  showall = TRUE  # Show all organ outlines for context
)
print(circulatory_plot$plot)
```


### Custom Color Configuration
#### Using RColorBrewer Palettes
```r
respiratory_plot <- OrgHeatmap(
  data = example_Data3,
  system = "respiratory",
  palette = "PuBuGn",           # RColorBrewer palette
  reverse_palette = TRUE,       # Reverse color order
  color_mid = "#87CEEB",        # Custom middle color
  organbar = TRUE,
  organbar_title = "Mean Value",
  title = "Respiratory System (PuBuGn Palette)"
)
```

#### Custom Gradient Colors
```r
custom_plot <- OrgHeatmap(
  data = example_Data3,
  color_low = "#F7FBFF",        # Light blue for low values
  color_high = "#08306B",       # Dark blue for high values
  color_mid = "#6BAED6",        # Medium blue for middle values
  organbar_low = "#FFF7BC",     # Light yellow for bar chart low
  organbar_high = "#D95F0E",    # Dark orange for bar chart high
  title = "Custom Color Gradient"
)
```


### Organ Name Mapping
```r
# Custom organ name standardization
custom_mapping <- c(
  "adrenal" = "adrenal_gland",
  "lymph node" = "lymph_node",
  "soft tissue" = "muscle"
)

mapped_plot <- OrgHeatmap(
  data = expr_data,
  organ_name_mapping = custom_mapping,
  value_col = "expression",
  title = "TP53 Expression with Custom Mapping"
)
```


### Custom Organ System Mapping
```r
# Extend default organ system mapping
prostate_organ_systems <- rbind(
  human_organ_systems,
  data.frame(
    organ = c("prostate", "bone", "lymph_node", "adrenal_gland"),
    system = c("reproductive", "musculoskeletal", "lymphatic", "endocrine"),
    stringsAsFactors = FALSE
  )
)

extended_plot <- OrgHeatmap(
  data = expr_data,
  organ_system_map = prostate_organ_systems,
  system = "reproductive",
  title = "Extended Organ System Mapping"
)
```


### Output and Saving
#### Save Plot and Data
```r
result <- OrgHeatmap(
  data = example_Data3,
  system = "circulatory",
  save_plot = TRUE,
  plot_path = file.path(getwd(), "circulatory_system.png"),
  plot_width = 12,
  plot_height = 10,
  plot_dpi = 300,
  plot_device = "png",
  save_clean_data = TRUE,
  clean_data_path = file.path(getwd(), "cleaned_data.rds")
)
```

#### Access Results
```r
# Access all returned components
print(result$plot)           # ggplot2 object
head(result$clean_data)      # Cleaned data frame
result$system_used          # System used for filtering
result$mapped_organs        # Standardized organ names
result$missing_organs       # Organs without coordinates
result$total_value          # Sum of all values
```


## Color Configuration Details
The package uses a unified color system with the following priority:
1.**Highest Priority**: organbar_low/organbar_high (bar chart colors)
2.**Medium Priority**: color_low/color_high/color_mid (heatmap colors)
3.**Lowest Priority**: palette with optional reverse_palette

### Supported Color Options
**RColorBrewer Palettes**: "YlOrRd", "PuBuGn", "Blues", etc.
**Viridis Palettes**: "viridis", "plasma", "magma", "inferno", "cividis"
**Custom Colors**: Any valid color name or hex code



## Parameter Reference
-`species`: "human", "mouse", or "organelle"
-`system`: Filter by organ system (not applicable for organelles)
-`palette`: RColorBrewer palette name for unified coloring
-`organbar`: Show/hide quantitative bar chart
-`showall`: Display all organ outlines for anatomical context
-`organ_name_mapping`: Standardize non-standard organ names
-`aggregate_method`: "mean", "sum", or "count" for duplicate organs



## Examples Dataset
The package includes comprehensive example datasets:
-`example_Data1`
-`example_Data2`
-`example_Data3`
-`expr_data`



## Troubleshooting

### Common Issues

#### Missing Organs
```r
# Check available organs in your species
names(human_organ_coord)  # For human
names(mouse_organ_coord)  # For mouse  
names(organelle_organ_coord)  # For organelles
```

#### Color Configuration
```r
# Validate RColorBrewer palette names
RColorBrewer::brewer.pal.info
```

#### Installation Issues
```r
# Install all dependencies manually if needed
install.packages(c("sf", "ggpolypath", "patchwork", "stringdist"))
```

#### Plot Not Generating
1.Ensure data has valid numeric values in the specified value column
2.Check that organ names match the coordinate data after standardization
3.Verify that the specified system contains organs with data



## Detailed Documentation
For comprehensive tutorials and parameter explanations:
```r
# Access function documentation
?OrgHeatmap

# View all package vignettes
browseVignettes("OrgHeatmap")
```


## Maintainer
- Qirui Shen

- Email: shenqr@i.smu.edu.cn

- GitHub: QiruiShen439

