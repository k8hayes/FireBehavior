# playing with CWD in tons/ha

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(tidyverse)
library(here)

cwd <- read.csv(here("data/CWD_output_nochar.csv"))

head(cwd)

ggplot(cwd, aes(x = as.factor(TREAT), y = TOTAL_MG.H)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Total Mg/Ha", title = "Total Mg/H")
