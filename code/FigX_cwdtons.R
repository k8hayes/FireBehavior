# playing with CWD in tons/ha

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(tidyverse)
library(here)

cwd <- read.csv(here("data/cwd_tons.h.csv"))

head(cwd)

one_plot <- ggplot(cwd, aes(x = as.factor(TREAT), y = X1HR_TONS.H, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Tons/Ha", title = "1-Hour Fuels")+ theme(legend.position = "none")

ten_plot <- ggplot(cwd, aes(x = as.factor(TREAT), y = X10HR_TONS.H, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Tons/Ha", title = "10-Hour Fuels") 

hund_plot <- ggplot(cwd, aes(x = as.factor(TREAT), y = X100HR_TONS.H, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Tons/Ha", title = "100-Hour Fuels") + theme(legend.position = "none")

thou_plot <- ggplot(cwd, aes(x = as.factor(TREAT), y = X1000HR_TONS.H, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Tons/Ha", title = "1000-Hour Fuels")

top <- plot_grid(ncol = 2, nrow = 1, one_plot, ten_plot, rel_widths = c(1,1.25),
                 labels = c("A.", "B."))
top
bottom <- plot_grid(ncol = 2, nrow = 1, hund_plot, thou_plot, rel_widths = c(1,1.25),
                    labels = c("C.", "D."))

plot_grid(top, bottom, nrow = 2, ncol = 1)
