# Figure X - used in presentations
# tree density

library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

dens <- read.csv(here("data/density.csv"), stringsAsFactors = F)

dens$SITE[dens$SITE == "DALTON"] <- "Upland"
dens$SITE[dens$SITE == "STEESE"] <- "Lowland"

plot_dens <- dens %>%
  group_by(SITE, TREAT, PLOT, DIV) %>%
  summarise(COUNT_HA = mean(COUNT_HA)) 

plot_dens %>%
  filter(TREAT > 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = COUNT_HA, fill = DIV)) + 
  geom_boxplot()  + ylim(0, 115000) + 
  labs(x = "Number of Fires", y = "Density (stems/ha)", 
       title = "Tree Density across Reburns") + 
  scale_fill_manual(values = c("#52BE80", "#F8C471"),
                    name = "Division",
                    labels = c("Conifer", "Deciduous")) + panel_border()
