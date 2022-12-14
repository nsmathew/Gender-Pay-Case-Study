
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

# -------*--------
# Figure 1 - to reorder later
# Nitin
# Box plot showing the difference in the average pay for mean and women across 5 
# the year period. We show the data separately for Pre and Post '92 unis
# Write-up - DRAFT, to update
# The graph shows how much of difference exists in the pay between men and 
# women in UK universities. The value shown is the average difference across the
# universities. blah blah blah
l_subtitle = "While we see improvements, there continues to be significant 
disparity in pay, more so for universities established prior to 1992"

payGapData %>%
  # Limits columns to only required, will use data for all 5 years
  select(EmployerName, DiffMeanHourlyPercent, pre92, year) %>%
  mutate(uni_type = recode_factor(pre92, 
                                 yes = "Pre '92",
                                 no = "Post '92")) %>%
  ggplot(aes(x = year, y = DiffMeanHourlyPercent / 100, fill = uni_type)) +
  # Remove outliers as they are not relevant for this plot
  geom_boxplot(outlier.shape=NA) +
  # For the appearance of the plot background
  theme_bw() +
  # Customise the scaling displayed for y-axis
  #scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  scale_y_continuous(labels = scales::percent) + 
  # Labels
  labs(x = "", y = "Avg difference in hourly pay(%)", 
       fill ="University\nCategory",
       title = "What is the trend in Gender Pay Gap in universities in the UK?",
       subtitle = gsub("\n", "", l_subtitle),
       caption = "Source: UK Government Gender Pay Data, 2018 to 2022") +
  # Annotation for UoS
  # Refer https://r-graph-gallery.com/42-colors-names.html for colour names
  annotate("point", x = "2017/18", y = 19 / 100,
           colour = "cornflowerblue", size = 2) +
  annotate("point", x = "2018/19", y = 17.9 / 100,
           colour = "cornflowerblue", size = 2) +
  annotate("point", x = "2019/20", y = 17.8 / 100,
           colour = "cornflowerblue", size = 2) +
  annotate("point", x = "2020/21", y = 16.5 / 100,
           colour = "cornflowerblue", size = 2) +
  annotate("point", x = "2021/22", y = 15.8 / 100,
           colour = "cornflowerblue", size = 2) +
  # For an arrow pointing to the last UoS annotation
  annotate(
    geom = "curve", x = 5.2, y = 22 / 100, xend = "2021/22", yend = 16 / 100, 
    colour = "cornflowerblue", curvature = .1, 
    arrow = arrow(length = unit(2, "mm"))
  ) +
  # For text for UoS
  annotate(geom = "text", x = 5, y = 24 / 100, label = "University of\n   Sheffield", 
           colour = "cornflowerblue", hjust = "left", size = 3) +
  #For pre92 colour
  # Refer https://r-graphics.org/recipe-colors-palette-discrete for palletes
  # We should try to use the same palletes across all plots
  scale_fill_brewer(palette = "Paired", breaks=c("Pre '92", "Post '92")) 


