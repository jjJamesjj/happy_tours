#Preparing data and drop variables
library(readr)
library(dplyr)
library(hms)
library(lubridate)
tour <- read_csv("C:/James Laptop/M.S. Applied Statistics/MAT 8480 Data Mining and Analytics - Dr. Zhang/Project/modeling_data.csv")
drop.vars <- c(grep("ID$|Trip_no|Grp_Size_Cat|^(Poor|Fair|Good|Excellent)", names(tour)))
tour %>% 
  mutate("Hotel_3orAbove" = Good_Hotels + Excellent_Hotels,
         "Meals_3orAbove" = Good_Meals + Excellent_Meals,
         "GUSS_3orAbove" = Good_GUSS + Excellent_GUSS,
         "Optionals_3orAbove" = Good_Optionals + Excellent_Optionals,
         "Bus_3orAbove" = Good_Buses + Excellent_Buses) %>% 
  select(-drop.vars, -contains("Gateway")) %>% 
  select(1:31, 59:63, everything()) -> tour

# Temporal variables
tour %>% select(1:2, 5:7, 13, 53:62, 63) -> james
james$FY <- as.factor(james$FY)
james$Tour_Season <- as.factor(james$Tour_Season)
james$Start_Day <- as.factor(james$Start_Day)
james$End_Day <- as.factor(james$End_Day)
james$Book_12Mo <- as.factor(james$Book_12Mo)

# Variables with missing values
na.vars <- names(james)[sapply(james, function(x) any(is.na(x)))]
na.vars  # ==> Variables indicating time of departing/arriving at domestic/international airports have missing values.

# Transform time into period of a day. I use 4 periods of a day.
james2 <- james[c(grep("_Time$", names(james)))]
for (i in 1:4){
james2[[i]] <- ifelse(as_hms(james2[[i]])>=as.hms("00:00:00") & as_hms(james2[[i]])<=as.hms("05:59:59"),"Night",
  ifelse(as_hms(james2[[i]])>=as.hms("06:00:00") & as_hms(james2[[i]])<=as.hms("11:59:59"),"Morning",
  ifelse(as_hms(james2[[i]])>=as.hms("12:00:00") & as_hms(james2[[i]])<=as.hms("17:59:59"),"Afternoon","Evening")))
}
james %>% select(-grep("_Time$", names(james))) %>% cbind(james2) -> james

# Break down TourDate into Date, Month, Year variables for better analysis
james %>% mutate(
  TourDay = day(as.POSIXlt(TourDate, format="%m/%d/%Y")),
  TourMonth = month(as.POSIXlt(TourDate, format="%m/%d/%Y")),
  TourYear = year(as.POSIXlt(TourDate, format="%m/%d/%Y"))
) %>% 
  # Drop FY because we already have TourYear, and drop TourDate
  select(-TourDate, -FY) -> james

# Transform tour day into tour week (week of a month), still a numerical variable 
james %>% mutate(
          "TourWeek" = ifelse(james$TourDay < 8, 1,
                      ifelse(james$TourDay < 15, 2,
                      ifelse(james$TourDay < 22, 3, 4)))) %>% 
          select(-TourDay) -> james

# Fix missing/null values masked as "-1" in some variables.
james %>% mutate(
  "Outbound_Connections" = ifelse(Outbound_Connections < 0 & Outbound_Connect_Time_Mins_2 > 0, 2,
                            ifelse(Outbound_Connections < 0 & Outbound_Connect_Time_Mins_1 > 0, 1,
                            ifelse(Outbound_Connections >= 0, Outbound_Connections, 0
                            )))) -> james
james %>% mutate(
  "Return_Connections" = ifelse(Return_Connections < 0 & Return_Connect_Time_Mins_2 > 0, 2,
                            ifelse(Return_Connections < 0 & Return_Connect_Time_Mins_1 > 0, 1,
                            ifelse(Return_Connections >= 0, Return_Connections, 0
                            )))) -> james

# Force negative values into NA/0 values depending on "Connections" variables to ensure logical imputation later
james$Outbound_Connect_Time_Mins_1 = ifelse(james$Outbound_Connections == 0, 0, 
                                      ifelse(james$Outbound_Connect_Time_Mins_1 <= 0, NA, james$Outbound_Connect_Time_Mins_1))
james$Outbound_Connect_Time_Mins_2 = ifelse(james$Outbound_Connections %in% c(0, 1), 0, 
                                      ifelse(james$Outbound_Connect_Time_Mins_2 <= 0, NA, james$Outbound_Connect_Time_Mins_2))
james$Return_Connect_Time_Mins_1 = ifelse(james$Return_Connections == 0, 0, 
                                      ifelse(james$Return_Connect_Time_Mins_1 <= 0, NA, james$Return_Connect_Time_Mins_1))
james$Return_Connect_Time_Mins_2 = ifelse(james$Return_Connections %in% c(0, 1), 0, 
                                      ifelse(james$Outbound_Connect_Time_Mins_2 <= 0, NA, james$Outbound_Connect_Time_Mins_2))

# Force extremely high values into NAs for later imputation of "more reasonable" values
james %>% select(contains("_Mins_")) %>% 
  sapply(function(x) quantile(x, .99, na.rm=T)) %>% max() -> cutoff.mins # Use 99th percentile as threshold
james %>% select(contains("_Mins_")) %>%
  sapply(function(x) ifelse(x > cutoff.mins, NA, x)) %>% 
  cbind(select(james, -contains("_Mins_"))) %>% 
  select(5:8, 18, 16:17, 12, 15, 9, 1:2, 14, 13, 10, 3:4, 11) -> james

# Export into a csv file
write.csv(james, file="C:/James Laptop/M.S. Applied Statistics/MAT 8480 Data Mining and Analytics - Dr. Zhang/Project/james.csv")
