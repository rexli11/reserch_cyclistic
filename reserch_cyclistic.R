library(tidyverse)
library(lubridate)
# ---------------------  ---------------------------------
# --------------------- Data Location ---------------------------------
setwd("D:/Github_version_file_R/data_set/clylistic_data")

# --------------------- Import Data Set ---------------------------------
# 2018_data
q1_18 <- read.csv("Divvy_Trips_2018_Q1.csv")
q2_18 <- read.csv("Divvy_Trips_2018_Q2.csv")
q3_18 <- read.csv("Divvy_Trips_2018_Q3.csv")
q4_18 <- read.csv("Divvy_Trips_2018_Q4.csv")

# 2019_data
q1_19 <- read.csv("Divvy_Trips_2019_Q1.csv")
q2_19 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_19 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_19 <- read.csv("Divvy_Trips_2019_Q4.csv")

# 2020_data
q1_20 <- read.csv("Divvy_Trips_2020_Q1.csv")

str(q1_18)
str(q1_19)
str(q2_19)
str(q3_19)
str(q4_19)
str(q1_20)
colnames(q1_18)
colnames(q1_19)
colnames(q2_19)
colnames(q3_19)
colnames(q4_19)
colnames(q1_20)
ncol(q2_19)

# 調整名稱與最新的data一致名稱
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


# 變更型態準備合併Data Set
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

# 透過rows合併季度資料
all_datas <- bind_rows(q2_19, q3_19, q4_19, q1_20)