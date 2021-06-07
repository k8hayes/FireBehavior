# heights of trees

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(here)

height <- read.csv(here("data/heights.csv"))

colnames(height)

ggplot(height, aes(x = as.factor(TREAT), y = HEIGHT_M, fill = SITE)) + 
  geom_boxplot() + labs(x = "Number of Fires", y = "Height (m)", title = "Indiv. Height Distributions")

site_names <- c('DALTON' = "Upland",
                'STEESE' = "Lowland") # comes from https://stackoverflow.com/questions/3472980/how-to-change-facet-labels

ggplot(height, aes(x = as.factor(TREAT), y = HEIGHT_M, fill = SPP)) + 
  geom_boxplot() + facet_wrap( ~ SITE, labeller = as_labeller(site_names) ) + 
  labs(x = "Number of Fires", y = "Height (m)", 
       title = "Species Height Distributions") + 
  scale_fill_manual(values = c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3",
                               "#c7eae5", "#80cdc1", "#35978f", "#01665e"),
                    name = "Species",
                    labels = c("Alder", "Dwarf Birch", "Birch",
                               "White Spruce", "Black Spruce", "Poplar",
                              "Aspen", "Willow"))
