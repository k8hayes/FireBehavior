# graphing fine fuel loads

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(tidyverse)
library(here)

fine <- read.csv(here("data/FFL_raw.csv"))

# fine[fine$HEIGHT > 300,]
# fine[fine$HEIGHT > 200,]

ggplot(fine, aes(x = as.factor(TREAT), y = HEIGHT, fill = SITE)) + 
  geom_boxplot() + 
  labs(x = "Number of Fires", y = "Height (cm)", title = "Fine Fuel Heights") + 
  scale_fill_manual(values = c("#d8b365", "#5ab4ac"),
                    labels = c("Upland", "Lowland"),
                    name = "Site")

test <- fine %>%
  group_by(SITE, TREAT, PLOT, LINE) %>%
  summarise(AV_height = mean(HEIGHT))

 ggplot(test, aes(x = as.factor(TREAT), y = AV_height, fill = SITE)) + 
  geom_boxplot() + 
  labs(x = "Number of Fires", y = "Height (cm)", title = "Fine Fuel Heights") + 
  scale_fill_manual(values = c("#d8b365", "#5ab4ac"),
                    labels = c("Upland", "Lowland"),
                    name = "Site")

 summary(fine$HEIGHT)

 