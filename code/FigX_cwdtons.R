# playing with CWD in tons/ha

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
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

# used this one for JFSP final report
ggplot(cwd_long, aes(x = as.factor(TREAT), y = Tons.H, fill = Fuel_Type)) + 
  geom_boxplot() + 
  scale_fill_manual(name = "Fuel Size Class",
                    values = c("#ffffb2", "#fecc5c","#fd8d3c","#e31a1c"),
                    labels = c("1-Hour", "10-Hour", "100-Hour", "1000-Hour")) + 
  labs(x = "Number of Fires", y = "Fuel Abundance (tons/ha)", 
       title = "Fuel Abundance across reburn history")

cwd_long %>%
  filter(TREAT != 0) %>%
  ggplot(aes(x = as.factor(TREAT), y = Tons.H, fill = Fuel_Type)) + 
  geom_boxplot() + panel_border() + 
  scale_fill_manual(name = "Fuel Size Class",
                    values = c("#ffeda0","#feb24c","#fd8d3c", "#e31a1c"),
                    labels = c("1-Hour", "10-Hour", "100-Hour", "1000-Hour")) + 
  labs(x = "Number of Fires", y = "Fuel Abundance (tons/ha)", 
       title = "Fuel Abundance across reburn history")

ggplot(cwd_long, aes(x = as.factor(TREAT), y = Tons.H, fill = group)) + 
  geom_boxplot() + facet_wrap(~SITE) + 
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
