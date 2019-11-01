#Preparing data and drop variables
library(readr)
library(dplyr)
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

# Allocating variables
## Temporal variables
tour %>% select(1:2, 5:7, 13, 53:62) -> james

## Demographical/geographical/info variables
tour %>% select(3:4, 8:12, 37:45) -> faith

## Evaluation variables
tour %>% select(22:36) -> irina

## Binary and some overall evaluation variables
tour %>% select(14:21, 46:52, 63) -> alex

