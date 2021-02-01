# graphing fine fuel loads

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(tidyverse)
library(here)

fine <- read.csv(here("data/FFL_raw.csv"))

ggplot(fine, aes(x = as.factor(TREAT), y = HEIGHT, fill = SITE)) + 
  geom_boxplot() + 
  labs(x = "Number of Fires", y = "Height (cm)", title = "Fine Fuel Loads") + 
  scale_fill_manual(values = c("#d8b365", "#5ab4ac"),
                    labels = c("Upland", "Lowland"),
                    name = "Site")
