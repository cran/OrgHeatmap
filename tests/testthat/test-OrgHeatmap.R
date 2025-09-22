test_that("基本功能测试 - 所有器官可视化", {
  test_data <- data.frame(
    organ = c("heart", "liver", "brain", "kidney"),
    value = c(15, 25, 20, 18)
  )
  
  result <- OrgHeatmap(test_data)
  
  # 验证输出结构
  expect_type(result, "list")
  expect_true("plot" %in% names(result))
  expect_s3_class(result$plot, c("gg", "ggplot"))
  
  # 验证数据清洗
  expect_true("clean_data" %in% names(result))
  expect_s3_class(result$clean_data, "data.frame")
  expect_true(nrow(result$clean_data) >= 1)
})



test_that("器官系统过滤 - 循环系统", {
  test_data <- data.frame(
    organ = c("heart", "liver", "artery", "brain"),
    value = c(10, 20, 15, 5)
  )
  expect_warning(
    result <- OrgHeatmap(test_data, system = "circulatory"),
    "The following organs have no coordinate data: artery",
    fixed = TRUE
  ) 
  
  # 验证系统过滤
  expect_true("system_used" %in% names(result))
  expect_equal(result$system_used, "circulatory")
  # 验证器官过滤
  expect_true(all(c("heart", "artery") %in% result$clean_data$organ))
})



test_that("器官名称映射功能", {
  test_data <- data.frame(
    organ = c("Heart Organ", "Hepatic Tissue", "Cerebral Cortex"),
    value = c(12, 18, 9)
  )
  
  custom_mapping <- c(
    "Heart Organ" = "heart",
    "Hepatic Tissue" = "liver",
    "Cerebral Cortex" = "brain"
  )
  
  result <- OrgHeatmap(test_data, organ_name_mapping = custom_mapping)
  
  # 验证映射结果
  expect_true(all(c("heart", "liver", "brain") %in% result$clean_data$organ))
})



test_that("器官名模糊匹配功能测试", {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    skip("stringdist包未安装，跳过模糊匹配测试")
  }
  
  test_data <- data.frame(
    organ = c("hearte", "livers", "brai"),
    value = c(10, 20, 15)
  )
  
  # 使用 expect_message 检查模糊匹配消息
  # 使用 regexp 参数而不是 pattern，并使用 all = TRUE 检查所有消息
  expect_message(
    {
      result_fuzzy <- OrgHeatmap(test_data)
    },
    regexp = "Fuzzy matched.*hearte.*heart|Fuzzy matched.*livers.*liver|Fuzzy matched.*brai.*brain",
    all = TRUE
  )
  
  # 验证模糊匹配结果
  standard_organs <- c("heart", "liver", "brain")
  expect_true(
    all(standard_organs %in% result_fuzzy$clean_data$organ),
    info = "模糊匹配后未生成正确的标准化器官名"
  )
})



test_that("聚合方法测试", {
  test_data <- data.frame(
    organ = c("heart", "heart", "liver", "brain", "brain", "brain"),
    value = c(10, 20, 15, 5, 10, 15)
  )
  
  # 1、测试mean聚合
  result_mean <- OrgHeatmap(test_data, aggregate_method = "mean")
  heart_val <- result_mean$clean_data$value[result_mean$clean_data$organ == "heart"]
  expect_equal(heart_val, 15)

  # 2、测试sum聚合
  result_sum <- OrgHeatmap(test_data, aggregate_method = "sum")
  heart_sum <- result_sum$clean_data$value[result_sum$clean_data$organ == "heart"]
  liver_sum <- result_sum$clean_data$value[result_sum$clean_data$organ == "liver"]
  brain_sum <- result_sum$clean_data$value[result_sum$clean_data$organ == "brain"] 
  expect_equal(heart_sum, 30)  # 10+20
  expect_equal(liver_sum, 15)  # 15
  expect_equal(brain_sum, 30)  # 5+10+15
  
  # 3、count聚合（统计器官出现次数，与value无关）
  result_count <- OrgHeatmap(test_data, aggregate_method = "count")
  heart_count <- result_count$clean_data$value[result_count$clean_data$organ == "heart"]
  liver_count <- result_count$clean_data$value[result_count$clean_data$organ == "liver"]
  brain_count <- result_count$clean_data$value[result_count$clean_data$organ == "brain"]  
  expect_equal(heart_count, 2)  # 出现2次
  expect_equal(liver_count, 1)  # 出现1次
  expect_equal(brain_count, 3)  # 出现3次
})



test_that("排序功能测试", {
  test_data <- data.frame(
    organ = c("heart", "liver", "brain"),
    value = c(15, 25, 10)
  )
  
  # 降序排序
  result_sorted <- OrgHeatmap(test_data, sort_by_value = TRUE)
  organs_sorted <- result_sorted$clean_data$organ
  expect_equal(organs_sorted[1], "liver")
})



test_that("条形图功能测试", {
  test_data <- data.frame(
    organ = c("heart", "liver", "brain"),
    value = c(12.3456, 18.9012, 9.8765)
  )
  
  # 默认条形图
  result_default <- OrgHeatmap(test_data, organbar = TRUE)
  expect_true("patchwork" %in% class(result_default$plot) || "gg" %in% class(result_default$plot))
})



test_that("错误输入处理", {
  bad_data2 <- data.frame(organ = "heart", value = 1)
  expect_error(
    OrgHeatmap(bad_data2, organ_col = "nonexistent"),
    "Specified organ column"
  )
})



test_that("缺失器官处理", {
  test_data <- data.frame(
    organ = c("heart", "unknown_organ1", "liver", "unknown_organ2"),
    value = c(10, 5, 15, 8)
  )
  
  expect_warning(
    result <- OrgHeatmap(test_data),
    # 正则：匹配两种顺序的缺失器官警告
    "The following organs have no coordinate data: (unknown_organ1.*unknown_organ2|unknown_organ2.*unknown_organ1)",
    fixed = FALSE,
    perl = TRUE
  )
  
  expect_true("missing_organs" %in% names(result))
  expect_length(result$missing_organs, 2)
  # 验证缺失器官列表包含两个未知器官（顺序无关）
  expect_setequal(result$missing_organs, c("unknown_organ1", "unknown_organ2"))
})



test_that("显示选项测试 - showall 和 outline", {
  test_data <- data.frame(
    organ = c("heart", "liver"),
    value = c(10, 15)
  )
  
  # 测试 showall = TRUE
  result_showall <- OrgHeatmap(test_data, showall = TRUE)
  expect_s3_class(result_showall$plot, "ggplot")
})



test_that("颜色选项测试", {
  test_data <- data.frame(
    organ = c("heart", "liver"),
    value = c(10, 20)
  )
  
  # 自定义颜色
  expect_silent(
    OrgHeatmap(
      test_data,
      fillcolor_outline = "lightblue"
    )
  )
})



test_that("颜色梯度方向参数（direction）", {
  test_data <- data.frame(organ = c("heart", "liver"), value = c(10, 20))
  # 测试direction=-1（反转梯度）
  expect_silent(
    OrgHeatmap(test_data, direction = -1, palette = "YlOrRd")
  )
})



test_that("无效颜色参数的错误处理", {
  test_data <- data.frame(organ = "heart", value = 10)
  # 测试无效颜色格式
  expect_error(
    OrgHeatmap(test_data, fillcolor_outline = "#1234"),  # 不完整的hex码
    regexp = "Invalid color format.*fillcolor_outline"  # 期待的错误信息（部分匹配）
  )
  expect_error(
    OrgHeatmap(test_data, color_high = "invalid_color"),  # 不存在的颜色名
    "Invalid color format",
    fixed = TRUE
  )
})



test_that("颜色配置功能测试（调色板/梯度/反转）", {
  test_data <- data.frame(
    organ = c("heart", "liver", "brain"),
    value = c(10, 20, 15)
  )
  
  # 1、RColorBrewer调色板 + 反转
  result_palette <- OrgHeatmap(
    test_data,
    palette = "PuBuGn",
    reverse_palette = TRUE,
    color_mid = "#87CEEB"  # 自定义中间色
  )
  expect_s3_class(result_palette$plot, c("gg", "ggplot"))  # 验证绘图未报错
  
  # 2、2色梯度（自定义low/high）
  result_2color <- OrgHeatmap(
    test_data,
    color_low = "#FFE4E1",
    color_high = "#DC143C"
  )
  expect_silent(result_2color)  # 验证无警告/错误
  
  # 3、条形图纯色配置
  result_bar_solid <- OrgHeatmap(
    test_data,
    organbar = TRUE,
    organbar_color = "skyblue"  # 条形图纯色
  )
  expect_true("patchwork" %in% class(result_bar_solid$plot) || "gg" %in% class(result_bar_solid$plot))
})



test_that("自定义器官系统映射（dataframe格式）", {
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
      organ_system_map = custom_system_map
    ),
    "The following organs have no coordinate data: custom_organ",
    fixed = TRUE
  )
  
  # 验证自定义系统
  expect_equal(result$system_used, "custom_system")
})



test_that("自定义器官系统映射（CSV文件路径）", {
  # 1. 创建临时CSV文件（自定义系统映射）
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
  
  # 2. 测试通过CSV路径传入映射
  test_data <- data.frame(
    organ = c("heart", "liver", "custom_organ"),
    value = c(10, 20, 15)
  )
  
  # custom_organ无坐标，应触发警告
  expect_warning(
    result_csv_map <- OrgHeatmap(
      test_data,
      system = "my_system",
      organ_system_map = temp_csv  # 传入CSV路径
    ),
    "The following organs have no coordinate data: custom_organ",
    fixed = TRUE
  )
  
  # 验证系统过滤结果
  expect_equal(result_csv_map$system_used, "my_system")
  expect_true(all(c("heart", "liver") %in% result_csv_map$clean_data$organ))
  
  # 清理临时文件
  unlink(temp_csv)
})



test_that("空输入和边界值测试", {
  # 空数据框
  expect_error(OrgHeatmap(data.frame()), "Input data cannot be empty (nrow(data) == 0).", fixed = TRUE)
  
  # 单器官测试
  expect_silent(OrgHeatmap(data.frame(organ = "heart", value = 10)))
  
  # 零值和负值
  expect_silent(OrgHeatmap(data.frame(organ = "heart", value = 0)))
  expect_silent(OrgHeatmap(data.frame(organ = "heart", value = -5)))
})



test_that("列名自定义测试", {
  test_data <- data.frame(
    custom_organ_col = c("heart", "liver"),
    custom_value_col = c(15, 25)
  )
  
  result <- OrgHeatmap(
    test_data,
    organ_col = "custom_organ_col",
    value_col = "custom_value_col"
  )
  
  # 验证自定义列名
  expect_true("custom_organ_col" %in% colnames(result$clean_data), 
              info = "自定义器官列名未保留在clean_data中")
  expect_true("custom_value_col" %in% colnames(result$clean_data), 
              info = "自定义数值列名未保留在clean_data中")
})



# 1、基础保存功能测试：PNG图片 + RDS数据（核心保存逻辑验证）
test_that("保存功能 - 基础PNG图片和RDS数据生成", {
  test_data <- data.frame(
    organ = c("heart", "liver", "brain"),
    value = c(15.6, 22.3, 18.9)
  )
  temp_dir <- tempdir()
  plot_path <- file.path(temp_dir, "basic_save_plot.png")
  data_path <- file.path(temp_dir, "basic_save_data.rds")
  
  result <- OrgHeatmap(
    data = test_data,
    save_plot = TRUE,
    plot_path = plot_path,
    save_clean_data = TRUE,
    clean_data_path = data_path,
    plot_width = 8,
    plot_height = 6,
    plot_dpi = 150
  )
  
  # 验证文件存在
  expect_true(file.exists(plot_path), info = "PNG图片未生成")
  expect_true(file.exists(data_path), info = "RDS数据未生成")
  
  # 读取保存的数据
  saved_data <- readRDS(data_path)
  
  # 忽略顺序验证器官名
  expect_setequal(
    saved_data$organ, 
    c("heart", "liver", "brain")
  )
  
  # 按器官名排序后验证数值（确保一一对应）
  saved_data_sorted <- saved_data[order(saved_data$organ), ]
  
  # 根据实际排序结果(brain, heart, liver)调整预期值，对应的值为18.9, 15.6, 22.3
  expected_values <- c(18.9, 15.6, 22.3)
  expect_equal(
    round(saved_data_sorted$value, 1), 
    expected_values
  )
  
  # 清理
  unlink(plot_path)
  unlink(data_path)
})



# 2、多格式兼容性测试：覆盖PDF/SVG/JPEG/TIFF（格式支持验证）
test_that("保存功能 - 多图片格式兼容性（PDF/SVG/JPEG/TIFF）", {
  test_data <- data.frame(organ = "heart", value = 10)  # 单器官简化测试数据
  temp_dir <- tempdir()
  valid_formats <- c("pdf", "svg", "jpeg", "tiff")  # 支持的格式列表
  
  # 循环测试每种格式
  for (fmt in valid_formats) {
    plot_path <- file.path(temp_dir, paste0("multi_format_plot.", fmt))
    
    # 验证“保存成功”的消息
    expect_message(
      OrgHeatmap(
        data = test_data,
        save_plot = TRUE,
        plot_path = plot_path,
        plot_device = fmt,
        plot_width = 6,
        plot_height = 4
      ),
      paste0("Plot saved to: ", plot_path),  # 匹配函数输出的保存路径消息
      fixed = TRUE  # 精确匹配路径字符串
    )
    
    # 验证对应格式文件生成
    expect_true(
      file.exists(plot_path), 
      info = paste0("多格式保存：", fmt, "格式文件未生成，路径：", plot_path)
    )
    
    # 即时清理当前格式文件（避免累积）
    unlink(plot_path)
  }
})



# 3、错误处理测试：无效格式
test_that("保存功能 - 无效参数的错误处理", {
  test_data <- data.frame(organ = "heart", value = 10)
  temp_dir <- tempdir()
  
  # 无效图片格式（非支持的格式列表）
  invalid_format_path <- file.path(temp_dir, "invalid_format.bad")
  expect_error(
    OrgHeatmap(
      data = test_data,
      save_plot = TRUE,
      plot_path = invalid_format_path,
      plot_device = "badformat"  # 不存在的格式
    ),
    "Invalid plot_device",  # 匹配函数内定义的错误关键词
    fixed = TRUE,
    info = "无效格式测试：未触发预期错误"
  )
  
  # 清理
  if (file.exists(invalid_format_path)) unlink(invalid_format_path)
})



# 4、目录自动创建测试：嵌套不存在的目录（便捷功能验证）
test_that("保存功能 - 自动创建嵌套目录", {
  test_data <- data.frame(
    organ = c("heart", "liver"),
    value = c(15, 25)
  )
  temp_dir <- tempdir()
  
  # 定义嵌套的不存在目录（如：temp/subdir1/subdir2/）
  nested_dir <- file.path(temp_dir, "auto_create_dir", "subdir1", "subdir2")
  plot_path <- file.path(nested_dir, "nested_plot.png")
  data_path <- file.path(nested_dir, "nested_data.rds")
  
  # 验证图片和数据的保存消息
  expect_message(
    result <- OrgHeatmap(
      data = test_data,
      save_plot = TRUE,
      plot_path = plot_path,
      save_clean_data = TRUE,
      clean_data_path = data_path
    ),
    paste0("Plot saved to: ", plot_path),
    fixed = TRUE
  )
  
  expect_true(
    dir.exists(nested_dir), 
    info = paste0("目录自动创建：嵌套目录未生成，路径：", nested_dir)
  )
  
  # 验证文件在嵌套目录中生成
  expect_true(file.exists(plot_path), 
              info = "目录自动创建：图片未保存到嵌套目录")
  expect_true(file.exists(data_path), 
              info = "目录自动创建：数据未保存到嵌套目录")
  
  # 清理：删除整个嵌套目录（递归删除）
  unlink(file.path(temp_dir, "auto_create_dir"), recursive = TRUE)
})


