# Figure X - used in presentations
# tree density

library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())
options(scipen = 9999)

dens <- read.csv(here("data/dbh_unclump.csv"), stringsAsFactors = F)

dens$SITE[dens$SITE == "DALTON"] <- "Upland"
dens$SITE[dens$SITE == "STEESE"] <- "Lowland"

dens$EXP_FACT <- 400

dens$EXP_FACT[dens$PLOT == "32_2"] <- 200
dens$EXP_FACT[dens$PLOT == "32_9"] <- 200
dens$EXP_FACT[dens$PLOT == "48_1"] <- 200
dens$EXP_FACT[dens$PLOT == "56_2"] <- 200
dens$EXP_FACT[dens$PLOT == "7_3"] <- 40
dens$EXP_FACT[dens$PLOT == "12_1"] <- 40
dens$EXP_FACT[dens$PLOT == "50_1"] <- 40
dens$EXP_FACT[dens$PLOT == "34_2"] <- 200

dens_plot <- dens %>%
  filter(CANOPY > 0) %>%
  group_by(SITE, TREAT, PLOT, SPP) %>%
  summarise(COUNT_PLOT = n(), EXP_FACT = min(EXP_FACT))

dens_plot$COUNT_M <- dens_plot$COUNT_PLOT / dens_plot$EXP_FACT

dens_plot %>%
  filter(TREAT > 0) %>%
  filter(SPP != "ARCTO") %>%
  filter(SPP != "PIGL") %>%
  filter(SPP != "POBA") %>%
  ggplot(aes(x = as.factor(TREAT), y = COUNT_M, fill = SPP)) + 
  geom_boxplot()  + ylim(0,1) + 
  labs(x = "Number of Fires", y = "Density (stems/m)", 
       title = "Tree Density across Reburns") + 
scale_fill_manual(values = c("#8c510a",  "#dfc27d",
                             "#c7eae5", "#35978f", "#01665e"),
                  name = "Species",
                  labels = c("Alder",  "Birch",
                             "Black Spruce",
                             "Aspen", "Willow")) + 
  facet_wrap(~SITE)

# averages ########################
dens_plot %>%
  group_by(SITE, TREAT) %>%
  summarise(mean(COUNT_M))
