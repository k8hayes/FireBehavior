# graphing fine fuel heights

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
# taking out NAs
fine <- fine %>%
  filter(HEIGHT != "NA")

test <- fine %>%
  group_by(SITE, TREAT, PLOT, LINE) %>%
  summarise(AV_height = mean(HEIGHT))

# bringing in interval
int <- read.csv(here("data/cwd_tons.h.csv"))
int <- int %>%
  select(c("PLOT", "Interval"))

int$PLOT %in% test$PLOT

test$PLOT[test$PLOT == "32_2  "] <- "32_2"

plot.names <- test$PLOT

plot.int <- 0

for(i in 1:length(plot.names)) {
  plot.int[i] <- int$Interval[int$PLOT == plot.names[i]]
}
plot.int

test$Interval <- plot.int

ggplot(test, aes(x = Interval, y = AV_height, col = SITE)) + 
  geom_jitter(width = 4) + 
  labs(x = "Number of Fires", y = "Height (cm)", title = "Fine Fuel Heights") + 
  scale_color_manual(values = c("#d8b365", "#5ab4ac"),
                    labels = c("Upland", "Lowland"),
                    name = "Site")


 