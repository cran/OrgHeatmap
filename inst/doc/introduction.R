## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  dev = "png",  
  dpi = 150
)

## ----setup--------------------------------------------------------------------
library(OrgHeatmap)

## ----data-preview, echo=FALSE-------------------------------------------------
# Preview example data structure
data_path <- system.file("extdata", "exampledata.Rdata", package = "OrgHeatmap")
load(data_path)
cat("Example Data 1 (Mouse):\n")
print(head(example_Data1))
cat("\nExample Data 3 (Human):\n")
print(head(example_Data3))

