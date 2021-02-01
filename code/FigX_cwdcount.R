# graphing CWD 
# just counts for now

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(tidyverse)
library(here)

cwd <- read.csv(here("data/cwd_count_raw.csv"))

cwd <- cwd %>%
  rename("TEN_HOUR" = "X10HOUR", "ONE_HOUR" = "X1HOUR", "HUND_HOUR" = "X100HOUR",
         "THOU_HOUR" = "X1000HOUR")

one_plot <- ggplot(cwd, aes(x = as.factor(TREAT), y = ONE_HOUR, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Count", title = "One-Hour Fuels") + 
  scale_fill_manual(values = c("#fc8d59", "#91bfdb"),
                    labels = c("Upland", "Lowland"),
                    name = "Site") + theme(legend.position = "none")

ten_plot <- ggplot(cwd, aes(x = as.factor(TREAT), y = TEN_HOUR, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Count", title = "Ten-Hour Fuels") + 
  scale_fill_manual(values = c("#fc8d59", "#91bfdb"),
                    labels = c("Upland", "Lowland"),
                    name = "Site") 

hund_plot <- ggplot(cwd, aes(x = as.factor(TREAT), y = HUND_HOUR, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Count", title = "Hundred-Hour Fuels") + 
  scale_fill_manual(values = c("#fc8d59", "#91bfdb"),
                    labels = c("Upland", "Lowland"),
                    name = "Site")+ theme(legend.position = "none")

thou_plot <- ggplot(cwd, aes(x = as.factor(TREAT), y = THOU_HOUR, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Count", title = "Thousand-Hour Fuels") + 
  scale_fill_manual(values = c("#fc8d59", "#91bfdb"),
                    labels = c("Upland", "Lowland"),
                    name = "Site")      

top <- plot_grid(ncol = 2, nrow = 1, one_plot, ten_plot, rel_widths = c(1,1.25),
                 labels = c("A.", "B."))
top
bottom <- plot_grid(ncol = 2, nrow = 1, hund_plot, thou_plot, rel_widths = c(1,1.25),
                    labels = c("C.", "D."))

plot_grid(top, bottom, nrow = 2, ncol = 1)          
