# comparing understory vegetation 
# trying to understand the variation between sites and plots

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(here)


veg_weight <- read.csv(here("data/understory_veg_weight_raw.csv"))

veg_weight$DIFF <-  veg_weight$WET_WEIGHT - veg_weight$DRY_WEIGHT / veg_weight$WET_WEIGHT

ggplot(veg_weight, aes(y = DIFF, fill = SITE)) + geom_boxplot() + 
  labs(y = "Normalized weight lost", title = "Difference in fuel moisture between sites", x = "Sites")

veg_weight %>%
  filter(SITE == "DALTON") %>%
  ggplot(aes(x = PLOT, y = DIFF, col = as.factor(TREAT))) + geom_point() + 
  scale_color_manual(name = "Number of fires", 
                     values = c("red", "blue", "red", "green")) + 
  labs(y = "Normalized weight lost", x = "Plot ID", 
       title = "Difference in fuel moisture across plots")




