# Load the data ----

library(tidyverse)

pay17 <- read_csv("data/UK Gender Pay Gap Data - 2017 to 2018.csv") 

pay18 <- read_csv("data/UK Gender Pay Gap Data - 2018 to 2019.csv") 

pay19 <- read_csv("data/UK Gender Pay Gap Data - 2019 to 2020.csv") 

pay20 <- read_csv("data/UK Gender Pay Gap Data - 2020 to 2021.csv") 

pay21 <- read_csv("data/UK Gender Pay Gap Data - 2021 to 2022.csv") 


universities <- read_csv("data/universities.csv")

# Join with the university data and combine ----

# For each pay gap dataset, will add the year, 
# and *discard* the EmployerName column
# This ensures EmployerName is drawn from the universities
# data frame only

pay17year <- pay17 %>%
  mutate(year = "2017/18") %>%
  select(-EmployerName) %>%
  inner_join(universities, by = "EmployerId")


pay18year <- pay18 %>%
  mutate(year = "2018/19") %>%
  select(-EmployerName) %>%
  inner_join(universities, by = "EmployerId")

pay19year <- pay19 %>%
  mutate(year = "2019/20") %>%
  select(-EmployerName) %>%
  inner_join(universities, by = "EmployerId")

pay20year <- pay20 %>%
  mutate(year = "2020/21") %>%
  select(-EmployerName) %>%
  inner_join(universities, by = "EmployerId")

pay21year <- pay21 %>%
  mutate(year = "2021/22") %>%
  select(-EmployerName) %>%
  inner_join(universities, by = "EmployerId")



payGapData <- rbind(pay17year, pay18year,
                    pay19year, pay20year,
                    pay21year)

# Summary statistics and data checks ----

## How many observations each year?

payGapData %>%
  count(year)

## How many universities reported in all five years?

payGapData %>%
  select(EmployerName, year, DiffMedianHourlyPercent) %>%
  pivot_wider(names_from = year, 
              values_from = DiffMedianHourlyPercent) %>%
  na.omit()

## Compute mean of DiffMedianHourlyPercent for each year
payGapData %>%
  group_by(year) %>%
  summarise(meanGap = mean(DiffMedianHourlyPercent))



## ...and split by institution

payGapData %>%
  group_by(year, pre92) %>%
  summarise(meanGap = mean(DiffMedianHourlyPercent), .groups = "drop") %>%
  pivot_wider(names_from = pre92, values_from = meanGap)

# Check against raw data ----

payGapData %>%
  filter(EmployerName=="UNIVERSITY OF SHEFFIELD") %>%
  select(year, DiffMeanHourlyPercent )

# Making a long format dataframe ----

## Might be useful if studying pay quartiles - 
## may wish to treat quartile (1st, 2nd, 3rd, 4th) as a variable

colnames(payGapData)

payGapDataLong <- payGapData %>%
  pivot_longer(DiffMeanHourlyPercent:FemaleTopQuartile, 
               names_to = "payGapMeasure",
               values_to = "value") %>%
  select(EmployerName, year, payGapMeasure, value, pre92)


