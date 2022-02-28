library(tidyverse)
library(lubridate)
# ---------------------  ---------------------------------
# --------------------- Data Location ---------------------------------
setwd("D:/Github_version_file_R/data_set/clylistic_data")

# --------------------- Import Data Set ---------------------------------
# 暫時不用的檔案 (no use data)
# 2018_data
# q1_18 <- read.csv("Divvy_Trips_2018_Q1.csv")
# q2_18 <- read.csv("Divvy_Trips_2018_Q2.csv")
# q3_18 <- read.csv("Divvy_Trips_2018_Q3.csv")
# q4_18 <- read.csv("Divvy_Trips_2018_Q4.csv")
# # 2019_data
# q1_19 <- read.csv("Divvy_Trips_2019_Q1.csv")

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

# --------------------- merge data set into a single file ---------------------------------
colnames(q2_19)
colnames(q3_19)
colnames(q4_19)
colnames(q1_20)

# 調整名稱與最新的data一致名稱 (rename consistent to the new data colnames)
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
        started_at = X01...Rental.Details.Local.Start.Time,
        ended_at = X01...Rental.Details.Local.End.Time,
        rideable_type = X01...Rental.Details.Bike.ID,
        start_station_id = X03...Rental.Start.Station.ID,
        start_station_name = X03...Rental.Start.Station.Name,
        end_station_id = X02...Rental.End.Station.ID,
        end_station_name = X02...Rental.End.Station.Name,
        member_casual = User.Type
    )


# 變更型態準備合併Data Set (change data type)
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

# 透過rows合併季度資料 merge data set by rows
all_datas <- bind_rows(q2_19, q3_19, q4_19, q1_20)

# 移除最新的檔案(2020)中沒有的項目 (Remove items is not in the 2020 data set)
all_datas <- all_datas %>%
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


# export merge data set
write.table(all_datas, file = "D:/Github_version_file_R/data_set/clylistic_data/all_datas.csv", sep = ",", row.names = TRUE, na = "NA")

# --------------------- Data Location ---------------------------------
setwd("D:/Github_version_file_R/data_set/clylistic_data")

# --------------------- Import Data Set ---------------------------------
all_datas <- read.csv("all_datas.csv")

# --------------------- clearn and fix data values ---------------------------------
# Check data values
colnames(all_datas) # 9cols
nrow(all_datas) # 3879822
dim(all_datas) # 一維數據框 dimensions 1
head(all_datas)
tail(all_datas) # also head()
str(all_datas)
summary(all_datas)

# 檢查member與start_station_name內的值 Check values in merber & start_station_name
all_datas_factor <- all_datas %>%
    mutate(
        member_casual = as.factor(member_casual),
        start_station_name = as.factor(start_station_name),
    )

summary(all_datas_factor)

# 重複命名的rows變更 >> Subscriber與Customer Fix duplicate naming in Subscriber & Customer
all_datas <- all_datas %>%
    mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))

# 變更日期格式，新增欄位月、日、年、周 Format date type in month、day、year、week
all_datas$date <- as.Date(all_datas$started_at)
all_datas$month <- format(as.Date(all_datas$date), "%m")
all_datas$day <- format(as.Date(all_datas$date), "%d")
all_datas$year <- format(as.Date(all_datas$date), "%Y")
all_datas$week <- format(as.Date(all_datas$date), "%A")

# 增加欄位，騎乘長度
all_datas$ride_length <- difftime(all_datas$ended_at, all_datas$started_at)
