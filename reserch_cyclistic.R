library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)

# ==========================================
# 觀看完整報告，請至雲端下載PDF : https://drive.google.com/drive/folders/1Dd62KlIexyLPgE6nYF0ZyCcUvnQzFf7p?usp=sharing
# View the full report, please go to the google cloud to download the PDF
# ==========================================

# ==========================================
# 1. Check Data
# ==========================================
# --------------------- Data Location ---------------------------------
setwd("D:/Github_version_file_R/data_set/clylistic_data")

# --------------------- import data set ---------------------------------
# 2019_data
q2_19 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_19 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_19 <- read.csv("Divvy_Trips_2019_Q4.csv")

# 2020_data
q1_20 <- read.csv("Divvy_Trips_2020_Q1.csv")

str(q2_19)
str(q3_19)
str(q4_19)
str(q1_20)
colnames(q2_19)
colnames(q3_19)
colnames(q4_19)
colnames(q1_20)


# ==========================================
# 2. Tidy Up Data And Combine (consistent the 2020 data set)
# ==========================================
# --------------------- merge data set into a single file ---------------------------------
# 調整名稱與2020數據一致 rename consistent to the 2020 data colnames
q4_19 <- q4_19 %>%
    rename(
        ride_id = trip_id,
        started_at = start_time,
        ended_at = end_time,
        rideable_type = bikeid,
        start_station_id = from_station_id,
        start_station_name = from_station_name,
        end_station_id = to_station_id,
        end_station_name = to_station_name,
        member_casual = usertype
    )

q3_19 <- q3_19 %>%
    rename(
        ride_id = trip_id,
        started_at = start_time,
        ended_at = end_time,
        rideable_type = bikeid,
        start_station_id = from_station_id,
        start_station_name = from_station_name,
        end_station_id = to_station_id,
        end_station_name = to_station_name,
        member_casual = usertype
    )

q2_19 <- q2_19 %>%
    rename(
        ride_id = X01...Rental.Details.Rental.ID,
        rideable_type = X01...Rental.Details.Bike.ID,
        started_at = X01...Rental.Details.Local.Start.Time,
        ended_at = X01...Rental.Details.Local.End.Time,
        start_station_name = X03...Rental.Start.Station.Name,
        start_station_id = X03...Rental.Start.Station.ID,
        end_station_name = X02...Rental.End.Station.Name,
        end_station_id = X02...Rental.End.Station.ID,
        member_casual = User.Type
    )

# 轉換數據型態準備整合 change data type before combine
q2_19 <- q2_19 %>%
    mutate(
        ride_id = as.character(ride_id),
        rideable_type = as.character(rideable_type)
    )

q3_19 <- q3_19 %>%
    mutate(
        ride_id = as.character(ride_id),
        rideable_type = as.character(rideable_type)
    )

q4_19 <- q4_19 %>%
    mutate(
        ride_id = as.character(ride_id),
        rideable_type = as.character(rideable_type)
    )

# 依照欄位進行合併 merge data set by rows
combine_datas <- bind_rows(q2_19, q3_19, q4_19, q1_20)
str(combine_datas)

# 移除最新的檔案(2020)中沒有的項目 Remove items is not in the 2020 data set
combine_datas <- combine_datas %>%
    select(-c(
        "X01...Rental.Details.Duration.In.Seconds.Uncapped",
        "Member.Gender",
        "X05...Member.Details.Member.Birthday.Year",
        "tripduration",
        "gender",
        "birthyear",
        "start_lat",
        "start_lng",
        "end_lat",
        "end_lng"
    ))

# 檢查合併後數據內容 Check data values
colnames(combine_datas) # 9cols
nrow(combine_datas) # 3879822
dim(combine_datas) # 一維數據框 dimensions 1
head(combine_datas)
tail(combine_datas)
str(combine_datas)
summary(combine_datas)

# 匯出第一份整理資料 export merge data set
write.table(combine_datas, file = "D:/Github_version_file_R/data_set/clylistic_data/combine_datas.csv", sep = ",", row.names = TRUE, na = "NA")

# ==========================================
# 3. Clearn Data Set
# ==========================================
# --------------------- Data Location ---------------------------------
setwd("D:/Github_version_file_R/data_set/clylistic_data")

# --------------------- Import Data Set ---------------------------------
combine_datas <- read.csv("combine_datas.csv")

# --------------------- clearn and fix data values ---------------------------------
# 檢查member內的值 Check values in merber in the table
table(combine_datas$member_casual)

# 重複命名的rows變更 >> Subscriber與Customer Fix duplicate naming in Subscriber & Customer
combine_datas <- combine_datas %>%
    mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))

# 變更日期格式，新增欄位月、日、年、周 Format date type in month、day、year、week
# 使用lubridate函數，也可以使用as.POSIXct進行日期調整
combine_datas <- combine_datas %>%
    mutate(
        year = year(combine_datas$started_at),
        month = month(combine_datas$started_at, label = TRUE),
        week = wday(combine_datas$started_at, label = TRUE),
        day = mday(combine_datas$started_at),
        hour = hour(combine_datas$started_at)
    )

# 檢查變更後的值 Check value
unique(combine_datas$year)
unique(combine_datas$month)
unique(combine_datas$week)
unique(combine_datas$day)
unique(combine_datas$hour)

# 重新給定星期的值 Change chinese to english in weeks
combine_datas <- combine_datas %>%
    mutate(week = recode(week, "週日" = "Sunday", "週一" = "Monday", "週二" = "Tuesday", "週三" = "Wednesday", "週四" = "Thursday", "週五" = "Friday", "週六" = "Saturday"))

# 重新給定月份的值 Change chinese to english in month
combine_datas <- combine_datas %>%
    mutate(month = recode(month, "一月" = "January", "二月" = "February", "三月" = "March", "四月" = "April", "五月" = "May", "六月" = "June", "七月" = "July", "八月" = "August", "九月" = "September", "十月" = "October", "十一月" = "November", "十二月" = "December"))

# 增加欄位 >> 騎乘長度(單位 : 秒)， Add Rows >> ride_length (in seconds)
combine_datas$ride_length <- difftime(combine_datas$ended_at, combine_datas$started_at)

# 轉換為數字作為統計使用 Change numeric type for statistics use
combine_datas$ride_length <- as.numeric(as.character(combine_datas$ride_length))

# 增加欄位 >> 騎乘長度(單位 : 分鐘)，Add Rows >> ride_length (in minutes)
combine_datas <- combine_datas %>%
    mutate(ride_length_minutes = (ride_length / 60))

# 轉換為數字作為統計使用 Change numeric type for statistics use
combine_datas$ride_length_minutes <- as.numeric(as.character(combine_datas$ride_length_minutes))

# 確認是否為數字 Check num type
is.numeric(combine_datas$ride_length_minutes)
is.numeric(combine_datas$ride_length)

# 移除錯誤數據 Remove bad data
combine_datas <- combine_datas[!(combine_datas$start_station_name == "HQ QR" | combine_datas$ride_length < 0 | combine_datas$ride_length_minutes < 0), ]

# 檢查是否移除乾淨 Check clearn or not
sum(combine_datas$start_station_name == "HQ QR")
sum(combine_datas$ride_length <= 0)
sum(combine_datas$ride_length_minutes <= 0)

# 存成新檔案 Save data sat as new file
combine_datas_clearn <- combine_datas

# 匯出第二份整理數據 export the clearn data set
write.table(combine_datas_clearn, file = "D:/Github_version_file_R/data_set/clylistic_data/combine_datas_clearn.csv", sep = ",", row.names = TRUE, na = "NA")

# ==========================================
# 4. Analyze Data information
# ==========================================
# --------------------- Data Location ---------------------------------
setwd("D:/Github_version_file_R/data_set/clylistic_data")

# --------------------- Import Data Set ---------------------------------
combine_datas_clearn <- read.csv("combine_datas_clearn.csv")
str(combine_datas_clearn)
table(combine_datas_clearn$member_casual)

# 星期進行排序 Sort Week
combine_datas_clearn$week <- ordered(combine_datas_clearn$week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# 月份進行排序 Sort month
combine_datas_clearn$month <- ordered(combine_datas_clearn$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

# 彙整總人數
aggregate(combine_datas_clearn$member_casual ~ combine_datas_clearn$ride_length_minutes, FUN = min) %>%
    view()

# 彙整用戶於週進行騎行比較 aggregation of week and ride length
aggregate(data = combine_datas_clearn, ride_length_minutes ~ member_casual + week, FUN = mean) %>%
    view()

# 彙整用戶於日進行騎行比較 aggregation of day and ride length
aggregate(data = combine_datas_clearn, ride_length_minutes ~ member_casual + day, FUN = mean) %>%
    view()

# 彙整用戶於月進行騎行比較 aggregation of month and ride length
aggregate(data = combine_datas_clearn, ride_length_minutes ~ member_casual + month, FUN = mean) %>%
    view()

# 彙整用戶於小時進行騎行比較 aggregation of hour and ride length
aggregate(data = combine_datas_clearn, ride_length_minutes ~ member_casual + hour, FUN = mean) %>%
    view()

# 使用者數量統計並顯示比例
user_sum <- combine_datas_clearn %>%
    group_by(member_casual) %>%
    summarise(total_riders = n()) %>%
    view()

# 計算使用者比例
per_rider <- paste(
    round(user_sum$total_riders / sum(user_sum$total_riders) * 100, digits = 2), "%"
) %>%
    view()

# 依照星期、型態分組進行騎乘者分析 Data aggregation by week and member type。
riders_weeks <- combine_datas_clearn %>%
    group_by(member_casual, week) %>%
    summarise(
        total_riders = n(),
        avg_length_minutes = mean(ride_length_minutes)
    ) %>%
    view()

# 會員與站點次數，取前50個與後50個站點 Summarize the number of sites used, and compare top 50 and last 50 sites
# Top Station
member_station_top <- combine_datas_clearn %>%
    group_by(start_station_name, member_casual) %>%
    summarise(
        total_riders = n(),
        avg_length_minutes = mean(ride_length_minutes)
    ) %>%
    arrange(desc(total_riders))

View(member_station_top[1:50, ])

# Bottom Station
member_station_bottom <- combine_datas_clearn %>%
    group_by(start_station_name, member_casual) %>%
    summarise(
        total_riders = n(),
        avg_length_minutes = mean(ride_length_minutes)
    ) %>%
    arrange(total_riders)

View(member_station_bottom[1:50, ])

# 以小時、會員總數進行觀察 Data aggregation by hour and total number of members
riders_day_of_hour <- combine_datas_clearn %>%
    group_by(member_casual, hour) %>%
    summarise(
        total_riders = n(),
        avg_length_minutes = mean(ride_length_minutes)
    ) %>%
    view()

# 以月、會員總數進行觀察 Data aggregation by month and total number of members
riders_month <- combine_datas_clearn %>%
    group_by(member_casual, month) %>%
    summarise(
        total_riders = n(),
        avg_length_minutes = mean(ride_length_minutes),
        avg_length_seconds = mean(ride_length)
    ) %>%
    arrange(month) %>%
    view()

# ==========================================
# 5. Visualization information
# ==========================================
# 使用者數量統計
user_sum %>%
    ggplot(mapping = aes(
        x = member_casual,
        y = total_riders,
        fill = member_casual
    )) +
    geom_col(position = "dodge") +
    scale_y_continuous(
        breaks = c(0, max(user_sum$total_riders), 500000),
        name = "Total Riders"
    ) +
    scale_x_discrete(
        name = "Member & Casual"
    ) +
    scale_fill_manual(values = c("tomato1", "springgreen3")) +
    labs(title = "使用者總人數比對 Comparison of the Total Number of Users")

# 轉化圓餅圖
pie_percent <- pie(user_sum$total_riders,
    labels = per_rider$x,
    col = c("yellow2", "violetred"),
    main = "年度會員與休閒會員比例 | Annual membership to the casual ratio"
)

# 添加說明
legend("topright", legend = c("Casual", "Member"), cex = 1, fill = c("yellow2", "violetred"))

# 依照星期進行騎乘者分析
riders_weeks %>%
    ggplot(mapping = aes(
        x = week,
        y = total_riders,
        fill = member_casual
    )) +
    geom_col(position = "dodge") +
    scale_y_continuous(
        breaks = c(seq(0, max(riders_weeks$total_riders), 50000)),
        name = "Total Riders"
    ) +
    scale_x_discrete(name = "Week Of Day") +
    scale_fill_manual(values = c("violetred4", "springgreen4")) +
    labs(
        title = "星期與使用者 Week & Riders",
        subtitle = "用戶數於週統計 Number of Users by Weeks"
    )

# 平均騎乘時間
riders_weeks %>%
    ggplot(mapping = aes(
        x = week,
        y = avg_length_minutes,
        fill = member_casual
    )) +
    geom_col(position = "dodge") +
    scale_y_continuous(
        name = "Average Length (Unit : Minutes)"
    ) +
    scale_x_discrete("Week Of Day") +
    scale_fill_manual(values = c("violetred4", "springgreen4")) +
    labs(
        title = "各使用者平均騎乘時間 Average Ride Length by User",
        subtitle = "統計於週的時間分布表 Statistical Time Distribution Table by Week"
    )

# ------------------------------------------
# 依照小時進行騎乘者分析
riders_day_of_hour %>%
    ggplot(mapping = aes(
        x = hour,
        y = total_riders,
        fill = member_casual
    )) +
    geom_col(position = "dodge") +
    scale_y_continuous(
        breaks = c(seq(0, max(riders_day_of_hour$total_riders), 50000)),
        name = "Total Riders"
    ) +
    scale_x_continuous(
        name = "Hours"
    ) +
    scale_fill_manual(values = c("royalblue3", "orange2")) +
    labs(
        title = "不同時段人數比較 The Number of Users in Different Time Periods",
        subtitle = " 使用者時段統計 User Period Statistics"
    )

# 平均騎乘時間
riders_day_of_hour %>%
    ggplot(mapping = aes(
        x = avg_length_minutes,
        y = total_riders,
        fill = member_casual
    )) +
    geom_violin() +
    scale_y_continuous(
        breaks = c(seq(0, max(riders_day_of_hour$total_riders), 50000)),
        name = "Total Riders"
    ) +
    scale_x_continuous(
        name = "Averge Length (Unit : Minute)"
    ) +
    scale_fill_manual(values = c("yellow3", "purple3")) +
    labs(title = "使用者統計比對騎乘時間 User Statistics vs. Ride Time")

# ------------------------------------------
# 依照月進行騎乘者分析
riders_month %>%
    ggplot(mapping = aes(
        x = month,
        y = total_riders,
        fill = member_casual
    )) +
    geom_col(position = "dodge") +
    scale_y_continuous(
        breaks = c(seq(0, max(riders_month$total_riders), 50000)),
        name = "Total Riders"
    ) +
    scale_x_discrete("Month", labels = abbreviate) + # 自動縮減labels長度
    scale_fill_manual(values = c("lightsalmon3", "cornflowerblue")) +
    labs(title = "以月份觀察使用者使用情形 Observing User Usage by Month")

# 平均騎乘時間
riders_month %>%
    ggplot(mapping = aes(
        x = avg_length_minutes,
        y = month,
        fill = member_casual
    )) +
    geom_col(position = "dodge") +
    xlab("Average Length (Unit : Minutes)") +
    ylab("Month") +
    scale_fill_manual(values = c("lightsalmon3", "cornflowerblue")) +
    labs(title = "使用者於各月份騎乘情形統計 Statistics of Users Situation in Month")

# ------------------------------------------
# 熱門站點使用狀況
member_station_top[1:500, ] %>%
    ggplot(mapping = aes(
        x = total_riders,
        y = avg_length_minutes,
        color = member_casual,
        shape = member_casual,
    )) +
    geom_point(position = "jitter") +
    facet_wrap(~member_casual) +
    scale_y_continuous(
        breaks = c(seq(0, max(member_station_top$avg_length_minutes), 10)),
        name = "Average Length (Unit : Minutes)"
    ) +
    xlab("Total Riders") +
    labs(title = "使用最多的500個站點使用情形 Top 500 sites used")

member_station_top[1:20, ] %>%
    ggplot(mapping = aes(
        x = start_station_name,
        y = total_riders,
        fill = member_casual
    )) +
    geom_col() +
    scale_y_continuous(
        breaks = c(seq(0, max(member_station_top$total_riders), 5000)),
        name = "Total Riders"
    ) +
    scale_x_discrete("Start Station (Unit : Top 20)", labels = abbreviate) +
    theme(axis.text.x = element_text(angle = 45)) +
    labs(title = "熱門站點使用者人數比對 Comparison of Users vs Popular Station")

# 冷門站點使用狀況
member_station_bottom[1:500, ] %>%
    ggplot(mapping = aes(
        x = total_riders,
        y = avg_length_minutes,
        color = member_casual,
        shape = member_casual
    )) +
    geom_point(position = "jitter") +
    theme(axis.text.x = element_text(angle = 30)) +
    facet_wrap(~member_casual) +
    scale_y_continuous(
        breaks = c(seq(0, max(member_station_bottom$avg_length_minutes), 1000)),
        name = "Average Length (Unit : Minutes)"
    ) +
    # ylab("Average Length (Unit : Minutes)") +
    xlab("Total Riders") +
    labs(title = "使用最少的500個站點使用情形 Minimum used 500 sites")

member_station_bottom[1:20, ] %>%
    ggplot(mapping = aes(
        x = start_station_name,
        y = total_riders,
        fill = member_casual
    )) +
    geom_col(position = "dodge") +
    scale_y_continuous(
        name = "Total Riders"
    ) +
    scale_x_discrete("Start Station", labels = abbreviate) +
    theme(axis.text.x = element_text(angle = 45)) +
    labs(title = "冷門站點使用者人數比對 Comparison of Users vs Unpopular Station")
# ------------------------------------------