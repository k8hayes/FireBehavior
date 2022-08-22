# heights of trees

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(here)

height <- read.csv(here("data/heights.csv"))

colnames(height)

height$CANOPY <- as.numeric(height$CANOPY)

ggplot(height, aes(x = as.factor(TREAT), y = HEIGHT_M, fill = SITE)) + 
  geom_boxplot() + labs(x = "Number of Fires", y = "Height (m)", title = "Indiv. Height Distributions")

site_names <- c('DALTON' = "Upland",
                'STEESE' = "Lowland") # comes from https://stackoverflow.com/questions/3472980/how-to-change-facet-labels

height %>%
  filter(SPP != "ARCTO") %>%
  filter(SPP != "PIGL") %>%
  filter(SPP != "POBA") %>%
  filter(CANOPY > 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = HEIGHT_M, fill = SPP)) + 
  geom_boxplot() + ylim(0,10) + facet_wrap(~SITE, labeller = as_labeller(site_names)) + 
  labs(x = "Number of Fires", y = "Height (m)", 
       title = "Species Height Distributions") + 
  scale_fill_manual(values = c("#8c510a",  "#dfc27d",
                               "#c7eae5", "#35978f", "#01665e"),
                    name = "Species",
                    labels = c("Alder",  "Birch",
                                "Black Spruce",
                              "Aspen", "Willow"))

# averages 
height <- na.omit(height)
av <- height %>%
  group_by(SITE,TREAT, SPP) %>%
  summarise(av = mean(HEIGHT_M)) 

av$av <- round(av$av, digits = 2)
