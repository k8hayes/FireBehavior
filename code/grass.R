# Grass cover

library(tidyverse)
library(here)
library(cowplot)
theme_set(theme_cowplot())


grass <- read.csv(here("data/gram_cover.csv"))

grass %>%
  filter(TREATMENT !=0) %>%
  ggplot(aes(x = factor(TREATMENT), y = GRAM, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "% Grass Cover", title = "Grass Cover")
  #scale_fill_manual(values = c("#d8b365", "#5ab4ac"),
                    labels = c("Upland", "Lowland"),
                    name = "Site")
