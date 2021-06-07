# Grass cover

library(tidyverse)
library(here)
library(cowplot)
theme_set(theme_cowplot())


grass <- read.csv(here("data/gram_cover.csv"))


ggplot(grass, aes(x = factor(TREATMENT), y = GRAM, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "% Grass Cover", title = "Grass Cover")
