# playing with CWD in tons/ha

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())
library(tidyverse)
library(here)

cwd <- read.csv(here("data/CWD_output_nochar.csv"))

head(cwd)

cwd$FINE_TONS.H <- cwd$FINE_MG.H*1.102
cwd$X3_TONS.H <- cwd$X3.SOUND_MG.H*1.102 + cwd$X3.ROTTEN_MG.H*1.102
cwd$TOTAL_TONS.H <- cwd$TOTAL_MG.H*1.102

cwd <- cwd %>%
  select(!c(X3.SOUND_MG.H, X3.ROTTEN_MG.H, FINE_MG.H, TOTAL_MG.H))

ggplot(cwd, aes(x = as.factor(TREAT), y = TOTAL_TONS.H, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Total Tons/Ha", title = "Total Tons/Ha")

ggplot(cwd, aes(x = as.factor(TREAT), y = FINE_TONS.H, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Total Tons/Ha", title = "Fine Tons/H")

ggplot(cwd, aes(x = as.factor(TREAT), y = X3_TONS.H, fill = SITE)) + geom_boxplot() + 
  labs(x = "Number of Fires", y = "Total Tons/Ha", title = "X3 Tons/H")
