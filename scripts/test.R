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
gpd_uni <- inner_join(gpd, uni, by =c("EmployerId"))
# Create a new column for assessment year by extracting DueDate's year
gpd_uni <- gpd_uni %>%
  mutate(AssmntYr = str_sub(DueDate, 1, 4))

# -------*--------
# Contingency Table - how many universities have provided data each year, for 
# both ‘pre-92’ and ‘post-92’ institutions
gpd_cont <- gpd_uni %>%
  group_by(AssmntYr, pre92) %>%
  summarise("UniCount" = n()) %>%
  arrange(desc(AssmntYr), desc(pre92))

# -------*--------
# Investigate how many universities have provided data for all four years. 
# Hints: 
# - think about what shape your data frame needs to be, and have another look at 
# Section 12.9 in the notes.
# - If each university is in one row of a data frame, the command na.omit() will 
# omit rows with any missing values.

# First group by EmployerName and AssmntYr to get a df with a summary view of
# the data
# Then perform a pivot_wider to convert the data to a single row per Uni and the
# AssmntYear as columns
# Post this remove any rows which ahve NA, i.e any uni which has not submitted
# data for all 5 years
all_yr_uni <- gpd_uni %>%
  group_by(EmployerName.y, AssmntYr) %>%
  summarise("UniCount" = n()) %>%
  arrange(EmployerName.y, desc(AssmntYr)) %>%
  pivot_wider(names_from = AssmntYr,
              values_from = UniCount) %>%
  na.omit()
# Finally print the EmployerName for list of Unis which have 5 year data
print(all_yr_uni[, c("EmployerName.y"), drop=FALSE], n=20)
  
# -------*--------
# Shows the trend for the Differen in Mean Hourly Rates from 2017-18 to 2021-22
# split by Pre and Post '92 unis as well as the rate of change from Y1 to Y5
gpd_uni %>%
  group_by(EmployerSize, pre92, AssmntYr) %>%
  summarize(mean_diffhrlypct = mean(DiffMeanHourlyPercent)) %>%
  pivot_wider(names_from = AssmntYr,
              values_from = mean_diffhrlypct) %>%
  mutate(ChgPct18to22 = (`2018`-`2022`)/`2018`*100) 
