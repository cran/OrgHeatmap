## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
   fig.width = 7,
  fig.height = 5,
  dev = "png",  
  dpi = 150
)

# 解决中文显示问题
if (.Platform$OS.type == "windows") {
  # Windows系统设置中文字体
  windowsFonts(sans = windowsFont("SimHei"))
} else if (Sys.info()["sysname"] == "Darwin") {
  # macOS系统设置中文字体
  ggplot2::theme_set(ggplot2::theme_minimal(base_family = "Heiti TC"))
} else {
  # Linux系统设置中文字体
  ggplot2::theme_set(ggplot2::theme_minimal(base_family = "WenQuanYi Micro Hei"))
}


## ----setup--------------------------------------------------------------------
library(OrgHeatmap)

