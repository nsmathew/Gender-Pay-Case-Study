# Import the data ----

source("scripts/makePayGapData.R")

head(payGapDataLong)
glimpse(payGapDataLong)

# Extracting the pay quartile data ----

unique(payGapDataLong$payGapMeasure)
payQuartiles <- unique(payGapDataLong$payGapMeasure)[c(8, 10, 12, 14)]

payGapDataLong %>% 
  filter(payGapMeasure %in% payQuartiles) 

# Basic box plot of pay quartile data ----

payGapDataLong %>% 
  filter(payGapMeasure %in% payQuartiles) %>%
  filter(year == "2017/18") %>%
  ggplot(aes(x = payGapMeasure, y = value, colour = pre92)) +
  geom_boxplot() 





         


