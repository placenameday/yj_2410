raw_data_path <- "/Users/placenameday/R study/yj_2410/data/demo/raw/00a17d6c125942fcadcdf62eeec74ae2_295量表.csv"
wideth <- 1920
height <- 1080

# 读取数据、重命名列名并转换指定列为数值型
data <- read_csv(raw_data_path, skip = 1) %>%
  rename_all(~ gsub("[/ ]", "_", .)) %>% # 替换列名中的 / 为 _
  mutate_at(vars(4:14), ~ as.numeric(.)) %>% # 将第 4 到第 14 列转换为数值型
  # 将 FixationX、FixationY 和 fixationDuration_s 列中数值为 -1 的值转换为 NA
  mutate_at(vars(FixationX, FixationY, fixationDuration_s), ~ na_if(., -1))

#
data <- data %>%
  mutate(stime = GazeTimestamp_ms, etime = lead(GazeTimestamp_ms)) %>%
  #
  mutate(
    axp = FixationX,
    ayp = FixationY
  ) %>%
  group_by(fixation_ser) %>%
  #
  mutate(
    stime = first(stime),
    etime = last(etime),
    dur = etime - stime,
    axp = first(axp) * wideth,
    ayp = first(ayp) * height,
    # aps 等于对应fixation_ser的LeftEyePupilRadius和RightEyePupilRadius的平均值
    aps = mean(c(LeftEyePupilRadius_px, RightEyePupilRadius_px), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(Id, QuestionsNum, fixation_ser, stime, etime, dur, axp, ayp, aps) %>%
  drop_na() %>%
  unique()
