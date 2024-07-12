## =============================================================================
## Author: Michelle Molina
## Email: Michelle@ConnectingEvidence.com
## Edits: 
## =============================================================================

# ==============================================================================
# 20240606  - KP Secondary Source - Poverty Status in the Past 12 Months
# ==============================================================================

# ==============================================================================
# clean around
# ==============================================================================
rm(list=ls()) # environment
graphics.off() # plotting devices
cat("\014") # console
# ==============================================================================

# ==============================================================================
# load the necessary packages (install if necessary)
# ==============================================================================

install.packages("tidycensus","reshape2","janitor")

library(tidyverse)
library(tidycensus)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(janitor)
library(data.table)

# ==============================================================================
#get census API
# ==============================================================================

census_api_key("11794acb51b5cdc0930db0eeb61df5358faa01fa", install = TRUE,overwrite=TRUE)

# ?census_api_key
# ?get_acs

# ==============================================================================
# Obtain the data from ACS
# ==============================================================================

#list variables wanted from census
var_list <- list(#"S1701_C01_001E", #PopTotal 
                 "DP04_0101E" ,  #HoUwithMortMed
                 "DP04_0109E",   #HoUwithoutMortMed
                 "DP04_0134E"   #OcpUnPayRent
                 #"S1701_C02_001E", #PovTotal 
                 #"S1701_C01_003E", #PopAgeUnder5
                 #"S1701_C02_003E", #PovAgeUnder5
                 #"S1701_C01_004E", #PopAge5to17
                 #"S1701_C02_004E", #PovAge5to17                  
                 #"S1701_C01_007E", #PopAge18to34
                 #"S1701_C02_007E", #PovAge18to34 
                 #"S1701_C01_008E", #PopAge35to64
                 #"S1701_C02_008E", #PovAge35to64 
                 #"S1701_C01_010E", #PopAge65Over
                 #"S1701_C02_010E",  #PovAge65Over                  
                 #"S1701_C01_011E", #PopSexM
                 #"S1701_C02_011E",  #PovSexM 
                 #"S1701_C01_012E", #PopSexF
                 #"S1701_C02_012E",  #PovSexF 
                 #"S1701_C01_014E", #PopRaceBlack
                 #"S1701_C02_014E",  #PovRaceBlack 
                 #"S1701_C01_015E", #PopRaceNative
                 #"S1701_C02_015E",  #PovRaceNative 
                 #"S1701_C01_016E", #PopRaceAsian
                 #"S1701_C02_016E",  #PovRaceAsian 
                 #"S1701_C01_017E", #PopRacePacificIslander 
                 #"S1701_C02_017E",  #PovRacePacificIslander 
                 #"S1701_C01_018E", #PopRaceOther 
                 #"S1701_C02_018E",  #PovRaceOther 
                 #"S1701_C01_019E", #PopRaceTwo 
                 #"S1701_C02_019E",  #PovRaceTwo 
                 #"S1701_C01_020E", #PopRaceLatino 
                 #"S1701_C02_020E",  #PovRaceLatino 
                 #"S1701_C01_021E", #PopRaceWhite 
                 #"S1701_C02_021E",  #PovRaceWhite 
                 #"S1701_C01_023E", #PopEdNoHS 
                 #"S1701_C02_023E",  #PovEdNoHS 
                 #"S1701_C01_024E", #PopEdHS 
                 #"S1701_C02_024E",  #PovEdHS 
                 #"S1701_C01_025E", #PopEdAS 
                 #"S1701_C02_025E",  #PovEdAS 
                 #"S1701_C01_026E", #PopEdBA 
                 #"S1701_C02_026E",  #PovEdBA
                 #"S1701_C01_035E", #PopWorkFull 
                 #"S1701_C02_035E",  #PovWorkFull
                 #"S1701_C01_036E", #PopWorkPart 
                 #"S1701_C02_036E",  #PovWorkPart
                 #"S1701_C01_037E", #PopWorkNone 
                 #"S1701_C02_037E"  #PovWorkNone
                 ) ### Update variables 


### 2022 Data 

#grab zip code variables for one year
Data2022 <- get_acs(
  geography ="zcta",
  variables = var_list,
  year = 2022,
  zcta = c("93901",
           "93905",
           "93906",
           "93908",
           "93926",
           "93927",
           "93930",
           "93933",
           "93940",
           "93943",
           "93944",
           "93955",
           "93960",
           "95012",
           "95076"),
  moe_level = 95
)

## NOTE: MOE = Margin Of Error 

#Make dataset Wide
Data2022 <- Data2022 %>% pivot_wider(names_from = variable, values_from = c("estimate", "moe"))

# Define a function to calculate median while removing NA values
median_na_rm <- function(x) {
  median(x, na.rm = TRUE)
}
# Define a function to calculate average while removing NA values
mean_na_rm <- function(x) {
  mean(x, na.rm = TRUE)
}

# Calculate the median for each column
medians <- apply(Data2022, 2, median_na_rm)

# Calculate the mean for each column
means <- sapply(Data2022, mean_na_rm)

# Add the medians and means as new rows to the data frame
Data2022 <- rbind(Data2022, medians, means)

# Display the updated data frame
print(Data2022)

# Rename the new rows for clarity

Data2022[nrow(Data2022), 1] <- "mean"
Data2022[nrow(Data2022)-1, 1] <- "median"

print(Data2022)

#add median column for the year 
#Data2022 <- rbind(Data2022, apply(Data2022, 2, median_na_rm))
#Data2022 <- Data2022 %>% rbind(Data2022, Medians = sapply(Data2022, median, na.rm = TRUE))
  # rbind(Data2022, Medians = apply(Data2022, 2, median, na.rm = TRUE))
                      

#add year to data
Data2022$year <- 2022


### 2021 Data 

#grab zip code variables for one year
Data2021 <- get_acs(
  geography ="zcta",
  variables = var_list,
  year = 2021,
  zcta = c("93901",
           "93905",
           "93906",
           "93908",
           "93926",
           "93927",
           "93930",
           "93933",
           "93940",
           "93943",
           "93944",
           "93955",
           "93960",
           "95012",
           "95076"),
  moe_level = 95
)

## NOTE: MOE = Margin Of Error 

#Make dataset Wide
Data2021 <- Data2021 %>% pivot_wider(names_from = variable, values_from = c("estimate", "moe"))

# Define a function to calculate median while removing NA values
median_na_rm <- function(x) {
  median(x, na.rm = TRUE)
}
# Define a function to calculate average while removing NA values
mean_na_rm <- function(x) {
  mean(x, na.rm = TRUE)
}

# Calculate the median for each column
medians <- apply(Data2021, 2, median_na_rm)

# Calculate the mean for each column
means <- sapply(Data2021, mean_na_rm)

# Add the medians and means as new rows to the data frame
Data2021 <- rbind(Data2021, medians, means)

# Display the updated data frame
print(Data2021)

# Rename the new rows for clarity
Data2021[nrow(Data2021), 1] <- "mean"
Data2021[nrow(Data2021)-1, 1] <- "median"


#add year to data
Data2021$year <- 2021


### 2020 Data 

#grab zip code variables for one year
Data2020 <- get_acs(
  geography ="zcta",
  variables = var_list,
  year = 2020,
  zcta = c("93901",
           "93905",
           "93906",
           "93908",
           "93926",
           "93927",
           "93930",
           "93933",
           "93940",
           "93943",
           "93944",
           "93955",
           "93960",
           "95012",
           "95076"),
  moe_level = 95
)

## NOTE: MOE = Margin Of Error 

#Make dataset Wide
Data2020 <- Data2020 %>% pivot_wider(names_from = variable, values_from = c("estimate", "moe"))

#add total column for the year 
# Define a function to calculate median while removing NA values
median_na_rm <- function(x) {
  median(x, na.rm = TRUE)
}
# Define a function to calculate average while removing NA values
mean_na_rm <- function(x) {
  mean(x, na.rm = TRUE)
}

# Calculate the median for each column
medians <- apply(Data2020, 2, median_na_rm)

# Calculate the mean for each column
means <- sapply(Data2020, mean_na_rm)

# Add the medians and means as new rows to the data frame
Data2020 <- rbind(Data2020, medians, means)

# Display the updated data frame
print(Data2020)

# Rename the new rows for clarity
Data2020[nrow(Data2020), 1] <- "mean"
Data2020[nrow(Data2020)-1, 1] <- "median"


#add year to data
Data2020$year <- 2020


### 2019 Data 

#grab zip code variables for one year
Data2019 <- get_acs(
  geography ="zcta",
  variables = var_list,
  year = 2019,
  state = 06,
  zcta = c("93901",
           "93905",
           "93906",
           "93908",
           "93926",
           "93927",
           "93930",
           "93933",
           "93940",
           "93943",
           "93944",
           "93955",
           "93960",
           "95012",
           "95076"),
  moe_level = 95
)

## NOTE: MOE = Margin Of Error 

#Make dataset Wide
Data2019 <- Data2019 %>% pivot_wider(names_from = variable, values_from = c("estimate", "moe"))

#add total column for the year 
# Define a function to calculate median while removing NA values
median_na_rm <- function(x) {
  median(x, na.rm = TRUE)
}
# Define a function to calculate average while removing NA values
mean_na_rm <- function(x) {
  mean(x, na.rm = TRUE)
}

# Calculate the median for each column
medians <- apply(Data2019, 2, median_na_rm)

# Calculate the mean for each column
means <- sapply(Data2019, mean_na_rm)

# Add the medians and means as new rows to the data frame
Data2019 <- rbind(Data2019, medians, means)

# Display the updated data frame
print(Data2019)

# Rename the new rows for clarity
Data2019[nrow(Data2019), 1] <- "mean"
Data2019[nrow(Data2019)-1, 1] <- "median"


#add year to data
Data2019$year <- 2019



### 2018 Data 

#grab zip code variables for one year
Data2018 <- get_acs(
  geography ="zcta",
  variables = var_list,
  year = 2018,
  state = "06",
  zcta = c("93901",
           "93905",
           "93906",
           "93908",
           "93926",
           "93927",
           "93930",
           "93933",
           "93940",
           "93943",
           "93944",
           "93955",
           "93960",
           "95012",
           "95076"),
  moe_level = 95
)

## NOTE: MOE = Margin Of Error 

#Make dataset Wide
Data2018 <- Data2018 %>% pivot_wider(names_from = variable, values_from = c("estimate", "moe"))


# Define a function to calculate median while removing NA values
median_na_rm <- function(x) {
  median(x, na.rm = TRUE)
}
# Define a function to calculate average while removing NA values
mean_na_rm <- function(x) {
  mean(x, na.rm = TRUE)
}

# Calculate the median for each column
medians <- apply(Data2018, 2, median_na_rm)

# Calculate the mean for each column
means <- sapply(Data2018, mean_na_rm)

# Add the medians and means as new rows to the data frame
Data2018 <- rbind(Data2018, medians, means)

# Display the updated data frame
print(Data2018)

# Rename the new rows for clarity
Data2018[nrow(Data2018), 1] <- "mean"
Data2018[nrow(Data2018)-1, 1] <- "median"


#add year to data
Data2018$year <- 2018


## NOTE zip code 93944 is not avaiable for 2020, 2019, nor 2018 

# ==============================================================================
# Combine data sets
# ==============================================================================

#put all data frames into list
df_list <- list(Data2022, Data2021, Data2020,Data2019,Data2018)

#merge all data frames in list
allData <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)

# ==============================================================================
# Added percentage of relevant variables 
# ==============================================================================

#Total 
#allData$totalPercent <- allData$estimate_S1701_C02_001 / allData$estimate_S1701_C01_001

#Ages 


#Sex
# allData$SexM <- allData$estimate_S1701_C02_011  / allData$estimate_S1701_C01_011 
# allData$SexF <- allData$estimate_S1701_C02_012  / allData$estimate_S1701_C01_012 
# 

#Race
# allData$RaceBlack <- allData$estimate_S1701_C02_014  / allData$estimate_S1701_C01_014 
# allData$RaceNative <- allData$estimate_S1701_C02_015  / allData$estimate_S1701_C01_015 
# allData$RaceAsian <- allData$estimate_S1701_C02_016  / allData$estimate_S1701_C01_016 
# allData$RacePacificIslander <- allData$estimate_S1701_C02_017  / allData$estimate_S1701_C01_017 
# allData$RaceOther <- allData$estimate_S1701_C02_018  / allData$estimate_S1701_C01_018 
# allData$RaceTwo <- allData$estimate_S1701_C02_019  / allData$estimate_S1701_C01_019 
# allData$RaceLatino <- allData$estimate_S1701_C02_020  / allData$estimate_S1701_C01_020 
# allData$RaceWhite <- allData$estimate_S1701_C02_021  / allData$estimate_S1701_C01_021 
# 
# #Ed
# allData$EdNoHS <- allData$estimate_S1701_C02_023  / allData$estimate_S1701_C01_023 
# allData$EdHS <- allData$estimate_S1701_C02_024  / allData$estimate_S1701_C01_024 
# allData$EdAS <- allData$estimate_S1701_C02_025  / allData$estimate_S1701_C01_025 
# allData$EdBA <- allData$estimate_S1701_C02_026  / allData$estimate_S1701_C01_026 
# 
# 
# #Work
# allData$WorkFull <- allData$estimate_S1701_C02_035  / allData$estimate_S1701_C01_035 
# allData$WorkPart <- allData$estimate_S1701_C02_036  / allData$estimate_S1701_C01_036 
# allData$WorkNone <- allData$estimate_S1701_C02_037  / allData$estimate_S1701_C01_037 

# ==============================================================================
# Remove columns that are not necessary  
# ==============================================================================

df_new <- allData %>% select(-contains("moe"))
df_new <- df_new %>% select(-contains("NAME"))

# ==============================================================================
# Rename columns to make it easier to manage 
# ==============================================================================

df_new <- df_new %>% rename(Housing_With_Mortgage_Estimate = estimate_DP04_0101,
                            Housing_Withou_Mortgage_Estimate = estimate_DP04_0109,
                            Occupied_Units_Pyaing_Rent_Estimate = estimate_DP04_0134,
                            #total_Estimate = estimate_S1701_C02_001,
                            #Age_Under5_Estimate = estimate_S1701_C02_003,
                            #Age_5to17_Estimate = estimate_S1701_C02_004,
                            #Age_18to34_Estimate = estimate_S1701_C02_007,
                            # Age_35to64_Estimate = estimate_S1701_C02_008,
                            # Age_65Above_Estimate = estimate_S1701_C02_010,
                            # Sex_Male_Estimate = estimate_S1701_C02_011,
                            # Sex_Female_Estimate = estimate_S1701_C02_012,
                            # Race_Black_Estimate = estimate_S1701_C02_014,
                            # Race_Native_Estimate = estimate_S1701_C02_015,
                            # Race_Asian_Estimate = estimate_S1701_C02_016,
                            # Race_PacificIslander_Estimate = estimate_S1701_C02_017,
                            # Race_Other_Estimate = estimate_S1701_C02_018,
                            # Race_TwoOrMore_Estimate = estimate_S1701_C02_019,
                            # Race_Latino_Estimate = estimate_S1701_C02_020,
                            # Race_White_Estimate = estimate_S1701_C02_021,
                            # Ed_NoHighSchool_Estimate = estimate_S1701_C02_023,
                            # Ed_HighSchool_Estimate = estimate_S1701_C02_024,
                            # Ed_Associates_Estimate = estimate_S1701_C02_025,
                            # Ed_Bachelor_Estimate = estimate_S1701_C02_026,
                            # Work_FullTime_Estimate = estimate_S1701_C02_035,
                            # WorkPartTim_Estimate = estimate_S1701_C02_036,
                            # Work_None_Estimate = estimate_S1701_C02_037,
                            # total_Total = estimate_S1701_C01_001,
                            # Age_Under5_Total = estimate_S1701_C01_003,
                            # Age_5to17_Total = estimate_S1701_C01_004,
                            # Age_18to34_Total = estimate_S1701_C01_007,
                            # Age_35to64_Total = estimate_S1701_C01_008,
                            # Age_65Above_Total = estimate_S1701_C01_010,
                            # Sex_Male_Total = estimate_S1701_C01_011,
                            # Sex_Female_Total = estimate_S1701_C01_012,
                            # Race_Black_Total = estimate_S1701_C01_014,
                            # Race_Native_Total = estimate_S1701_C01_015,
                            # Race_Asian_Total = estimate_S1701_C01_016,
                            # Race_PacificIslander_Total = estimate_S1701_C01_017,
                            # Race_Other_Total = estimate_S1701_C01_018,
                            # Race_TwoOrMore_Total = estimate_S1701_C01_019,
                            # Race_Latino_Total = estimate_S1701_C01_020,
                            # Race_White_Total = estimate_S1701_C01_021,
                            # Ed_NoHighSchool_Total = estimate_S1701_C01_023,
                            # Ed_HighSchool_Total = estimate_S1701_C01_024,
                            # Ed_Associates_Total = estimate_S1701_C01_025,
                            # Ed_Bachelor_Total = estimate_S1701_C01_026,
                            # Work_FullTime_Total = estimate_S1701_C01_035,
                            # WorkPartTime_Total = estimate_S1701_C01_036,
                            # Work_None_Total = estimate_S1701_C01_037,
                            # 
                            # total_Percent = totalPercent,
                            # Age_Under5_Percent = ageUnder5,
                            # Age_5to17_Percent = Age5to17,
                            # Age_18to34_Percent = Age18to34,
                            # Age_35to64_Percent = Age35to64,
                            # Age_65Above_Percent = Age65,
                            # Sex_Male_Percent = SexM,
                            # Sex_Female_Percent = SexF,
                            # Race_Black_Percent = RaceBlack,
                            # Race_Native_Percent = RaceNative,
                            # Race_Asian_Percent = RaceAsian,
                            # Race_PacificIslander_Percent = RacePacificIslander,
                            # Race_Other_Percent = RaceOther,
                            # Race_TwoOrMore_Percent = RaceTwo,
                            # Race_Latino_Percent = RaceLatino,
                            # Race_White_Percent = RaceWhite,
                            # Ed_NoHighSchool_Percent = EdNoHS,
                            # Ed_HighSchool_Percent = EdHS,
                            # Ed_Associates_Percent = EdAS,
                            # Ed_Bachelor_Percent = EdBA,
                            # Work_FullTime_Percent = WorkFull,
                            # Work_PartTime_Percent = WorkPart,
                            # Work_None_Percent = WorkNone,
                            ZipCode = GEOID)

#colnames(df_new)

# ==============================================================================
# MakeLong
# ==============================================================================

df_new <- melt(setDT(df_new), id.vars = c("ZipCode","year"), varia?nble.name = "Category")
print(df_new)


# ==============================================================================
# Add column of type
# ==============================================================================

#df_new$Type <- ifelse(grepl("Percent", df_new$Category), "Percent", 
                     # ifelse(grepl("Total", df_new$Category), "Total","Estimate"))

# ==============================================================================
# Remove String for 
# ==============================================================================
# 
# df_new2 <- df_new %>%
#   mutate_at("Category", str_replace, "_Percent", "")
# 
# df_new2 <- df_new2 %>%
#   mutate_at("Category", str_replace, "_Estimate", "")
# 
# df_new2 <- df_new2 %>%
#   mutate_at("Category", str_replace, "_Total", "")

# ==============================================================================
# Print to Sheet 
# ==============================================================================
install.packages("googlesheets4")
library(googlesheets4)

# get authentiction
gs4_auth()
#sheet <- gs4_create("MedianRent")

MedianRent <- gs4_get("https://docs.google.com/spreadsheets/d/1QI9Q6PmsQxkLQCSrZrdOslrYzhKDlkaePtl6mibcGoY/edit?gid=0#gid=0")
# Assuming your data frame is named df
sheet_write(df_new, ss = MedianRent, sheet = "MedianRent")

# MedianRent  <- "https://docs.google.com/spreadsheets/d/1QI9Q6PmsQxkLQCSrZrdOslrYzhKDlkaePtl6mibcGoY/edit#gid=0" #copy link of spreadsheet
# MedianRent <- "Median"
# # Poverty  <- "https://docs.google.com/spreadsheets/d/1QI9Q6PmsQxkLQCSrZrdOslrYzhKDlkaePtl6mibcGoY/edit#gid=0" #copy link of spreadsheet
# # PovertySheet <- "Poverty"
# 
# if (!drive_has_token()) {
#   drive_auth()
#   gs4_auth(token = drive_token())
# }
# 
# #This overwrites what is on the tab in the actual google sheet
# sheet_write(df_new, 
#             ss=MedianRent,
#             sheet=MedianRent)
