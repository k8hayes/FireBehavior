# comparing understory vegetation 
# trying to understand the variation between sites and plots

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(here)


veg_weight <- read.csv(here("data/understory_veg_weight_raw.csv"))

veg_weight$DIFF <- veg_weight$DRY_WEIGHT / veg_weight$WET_WEIGHT

ggplot(veg_weight, aes(y = DIFF, fill = SITE)) + geom_boxplot()

veg_weight %>%
  filter(SITE == "DALTON") %>%
  ggplot(aes(x = PLOT, y = DIFF)) + geom_point()



