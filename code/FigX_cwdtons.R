# playing with CWD in tons/ha

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(tidyverse)
library(here)

cwd <- read.csv(here("data/cwd/cwd_tons.h.csv"))

head(cwd)

# testing interval


cwd_long <- cwd %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Fuel_Type",
               values_to = "Tons.H")


cwd_long$group <- NA
cwd_long$group[cwd_long$Fuel_Type == "X1HR_TONS.H"] <- "Fine"
cwd_long$group[cwd_long$Fuel_Type == "X10HR_TONS.H"] <- "Fine"
cwd_long$group[cwd_long$Fuel_Type == "X100HR_TONS.H"] <- "Medium"
cwd_long$group[cwd_long$Fuel_Type == "X1000HR_TONS.H"] <- "Large"

ggplot(cwd_long, aes(x = as.factor(TREAT), y = Tons.H, fill = group)) + 
  geom_boxplot() + 
  scale_fill_manual(name = "Fuel Size",
                    values = c("#ffeda0","#feb24c","#f03b20"),
                    labels = c("Fine", "Medium", "Large")) + 
  labs(x = "Number of Fires", y = "Fuel Abundance (tons/ha)", 
       title = "Fuel Abundance across reburn history")

one_plot <- ggplot(cwd, aes(x = Interval, y = X1HR_TONS.H, col = SITE))  + geom_jitter(width = 4) +
  labs(x = "Years since initial fire", y = "Tons/Ha", title = "1-Hour Fuels") + 
  scale_color_manual(name = "Site", 
                     values = c("#feb24c","#0570b0"),
                     labels = c("Upland", "Lowland")) + theme(legend.position = "none")

ten_plot <- ggplot(cwd, aes(x = Interval, y = X10HR_TONS.H, col = SITE))  + geom_jitter(width = 4) +
  labs(x = "Years since initial fire", y = "Tons/Ha", title = "10-Hour Fuels") + 
  scale_color_manual(name = "Site", 
                     values = c("#feb24c","#0570b0"),
                     labels = c("Upland", "Lowland"))

hund_plot <- ggplot(cwd, aes(x = Interval, y = X100HR_TONS.H, col = SITE))  + geom_jitter(width = 4) +
  labs(x = "Years since initial fire", y = "Tons/Ha", title = "100-Hour Fuels") + theme(legend.position = "none") +
  scale_color_manual(name = "Site", 
                     values = c("#feb24c","#0570b0"),
                     labels = c("Upland", "Lowland"))

thou_plot <- ggplot(cwd, aes(x = Interval, y = X1000HR_TONS.H, col = SITE))  + geom_jitter(width = 4) +
  labs(x = "Years since initial fire", y = "Tons/Ha", title = "1000-Hour Fuels") + 
  scale_color_manual(name = "Site", 
                     values = c("#feb24c","#0570b0"),
                     labels = c("Upland", "Lowland"))

top <- plot_grid(ncol = 2, nrow = 1, one_plot, ten_plot, rel_widths = c(1,1.25),
                 labels = c("A.", "B."))
top
bottom <- plot_grid(ncol = 2, nrow = 1, hund_plot, thou_plot, rel_widths = c(1,1.25),
                    labels = c("C.", "D."))

plot_grid(top, bottom, nrow = 2, ncol = 1)
