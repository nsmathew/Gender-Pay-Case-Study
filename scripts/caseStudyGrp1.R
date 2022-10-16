# -------*--------
# Load libraries
library(tidyverse)

# -------*--------
# Import data
gpd_2122 <- read_csv("data/UK Gender Pay Gap Data - 2021 to 2022.csv")
gpd_2021 <- read_csv("data/UK Gender Pay Gap Data - 2020 to 2021.csv")
gpd_1920 <- read_csv("data/UK Gender Pay Gap Data - 2019 to 2020.csv")
gpd_1819 <- read_csv("data/UK Gender Pay Gap Data - 2018 to 2019.csv")
gpd_1718 <- read_csv("data/UK Gender Pay Gap Data - 2017 to 2018.csv")
uni <- read_csv("data/universities.csv")

# -------*--------
# Combine data
# Combine gpd
gpd <- rbind(gpd_2122, gpd_2021, gpd_1920, gpd_1819, gpd_1718)
# Join the universities data. Use inner join to drop any rows not in Uni
# Remove the EmployerName from GPD data as it is available in uni data
payGapData <- gpd %>%
  select(-EmployerName) %>%
  inner_join(uni, by =c("EmployerId"))
# Create a new column for assessment year by extracting DueDate's year
payGapData <- payGapData %>%
  mutate(year = case_when(str_sub(DueDate, 1, 4) == '2022' ~ '2021/22',
                          str_sub(DueDate, 1, 4) == '2021' ~ '2020/21',
                          str_sub(DueDate, 1, 4) == '2020' ~ '2019/20',
                          str_sub(DueDate, 1, 4) == '2019' ~ '2018/19',
                          str_sub(DueDate, 1, 4) == '2018' ~ '2017/18'))



# Shows the trend for the Difference in Mean Hourly Rates from 2017-18 to 2021-22
# split by Pre and Post '92 unis as well as the rate of change from Y1 to Y5
payGapData %>%
  group_by(EmployerSize, pre92, year) %>%
  summarize(mean_diffhrlypct = mean(DiffMeanHourlyPercent)) %>%
  pivot_wider(names_from = year,
              values_from = mean_diffhrlypct) %>%
  mutate(ChgPct18to22 = (`2017/18`-`2021/22`)/`2017/18`*100) 
