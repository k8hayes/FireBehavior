# comparing understory vegetation 
# trying to understand the variation between sites and plots

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(here)

veg_weight <- read.csv(here("data/understory_veg_weight_raw.csv"))

veg_weight$DIFF <-  veg_weight$WET_WEIGHT - veg_weight$DRY_WEIGHT / veg_weight$WET_WEIGHT

# averages for model
veg_weight <- drop_na(veg_weight)

veg_weight$moist <-  veg_weight$WET_WEIGHT - veg_weight$DRY_WEIGHT 


veg_weight %>%
  group_by(TREAT) %>%
  summarise(AV = mean(moist), SD = sd(moist))

veg_weight %>%
  group_by(TREAT) %>%
  filter(TREAT == 1 | TREAT == 3) %>%
  summarise(quantile(DRY_WEIGHT, probs = c(0.25)))

# Plots ##########################################3
ggplot(veg_weight, aes(y = DIFF, x = as.factor(TREAT), fill = SITE)) + geom_boxplot() + 
  labs(y = "Normalized weight lost", title = "Difference in fuel moisture between sites", x = "Sites")

veg_weight %>%
  filter(SITE == "DALTON") %>%
  ggplot(aes(x = PLOT, y = DIFF, col = as.factor(TREAT))) + geom_point() + 
  scale_color_manual(name = "Number of fires", 
                     values = c("red", "blue", "red", "green")) + 
  labs(y = "Normalized weight lost", x = "Plot ID", 
       title = "Difference in fuel moisture across plots")


