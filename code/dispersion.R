# Species distances #################
# random distance from point to nearest individual of each spp present on plot

# Set up ###############################
library(tidyverse)
library(cowplot)
library(here)
theme_set(theme_cowplot())

# # pulls data in from csv file, assigns it as an object
# dalt <- read.csv(here("data/distances/Dalton_spp_distance_2021.csv"))
# stee <- read.csv(here("data/distances/Steese_spp_dist_2021.csv"))
# 
# # Data checks ############################
#  
#  # tells me how r is viewing the meter data
#   class(dalt$Distance_m)
#   class(stee$Distance_m)
# 
#   # summarizes the data for distance in feet (mean, min, max, etc)
#   summary(dalt$Distance_ft)
# 
#   # tells me unique entries for species column
#   unique(dalt$Species) # good way of checking if i've entered columns correctly
#   unique(stee$Species)
#   
#   dalt$Species[dalt$Species == "UKN"] <- "UNK"
# 
#   # check plot names
#   unique(dalt$Plot)
#   unique(stee$Plot)
# 
#   # checks length of plot list
#   length(unique(dalt$Plot)) # should be 22
#   length(unique(stee$Plot)) # should be 19
# 
#   # checking the column names for both data frames
#   colnames(dalt)
#   colnames(stee)
# 
# # Data changes #####################
#   
#   # combining data files!! 
#   combined <- rbind(dalt,stee)
# 
#   # converting feet to meters
#   combined$Distance_m[is.na(combined$Distance_ft) == FALSE] <- combined$Distance_ft[is.na(combined$Distance_ft) == FALSE] * 0.3048
#   
#   # renaming column
#   combined <- combined %>%
#     rename("Live_Dead" = "Live.Dead",
#            "TREAT" = 'Fires', "SITE" = "Site", "PLOT" = "Plot",
#            "SPP" = "Species", "DIST_M" = "Distance_m") %>%
#     select(!Distance_ft) # drop extra column
# 
#   # exporting combined file
#   write.csv(combined, "data/distances/Spp_dist_2021.csv", row.names = F)
#   
#   data <- combined
#   rm(stee,dalt,combined)
  
  data <- read.csv(here("data/distances/Spp_dist_2021.csv"))
  
# Results ###############
  
  # graphing all species distances
  data %>%
    filter(SPP != "UNK") %>%
    filter(DIST_M != "NA") %>%
    ggplot(aes(x = as.factor(TREAT), y = DIST_M, fill = SPP)) + 
    geom_boxplot() + facet_wrap(~SITE) + 
    labs(x = "Number of Fires", 
                          y = "Distance (meters)",
                          title = "Distance between Species") + 
    scale_fill_manual(name = "Species",
                      labels = c("Alder", "Dwarf Birch", "Birch", "Spruce", "Aspen",
                                 "Willow"),
                      values = c("#a6cee3","#1f78b4","#b2df8a",
                                 "#33a02c","#fb9a99","#e31a1c"))
  # Plotting according to Plot
  plot <- data %>%
    filter(DIST_M != "NA") %>%
    group_by(SITE, TREAT, PLOT) %>%
    summarise(AV = mean(DIST_M), SD = sd(DIST_M))
  
  plot %>%
    ggplot(aes(x = as.factor(TREAT), y = AV)) + 
    geom_boxplot()  + 
    labs(x = "Number of Fires", 
         y = "Distance (meters)",
         title = "Plot-level dispersion") 
  
  # checking outlier
  # data[data$DIST_M > 30,] # error - it's 32.5 m but supposed to be feet
  data[data$DIST_M > 20,] # these are real
  
  
  # Eberhardt's statistic #############################
  frame <- data %>%
    group_by(SITE, TREAT, PLOT, Frame) %>%
    summarise(min = min(DIST_M)) %>%
    group_by(SITE, TREAT, PLOT) %>%
    summarise(AV = mean(min), SD = sd(min))
  
  frame$EB <- (frame$SD/frame$AV)^2 + 1

  # with site
  ggplot(frame, aes(x = as.factor(TREAT), y = EB, fill = SITE)) + 
    geom_boxplot() + 
    labs(x = "Number of Fires")
  
  # without site
  ggplot(frame, aes(x = as.factor(TREAT), y = EB)) + geom_boxplot() + labs(x = "Number of Fires") 

  # according to species  
  spp <- data %>%
    filter(DIST_M != "NA") %>%
    group_by(SITE, TREAT, PLOT, SPP, Frame) %>%
    summarise(min = min(DIST_M)) %>%
    group_by(SITE, TREAT, PLOT, SPP) %>%
    summarise(AV = mean(min), SD = sd(min))
  
  spp$EB <- (spp$SD/spp$AV)^2 + 1
  
  # with site
  ggplot(spp, aes(x = as.factor(TREAT), y = EB)) + 
    geom_boxplot() + 
    labs(x = "Number of Fires")
  
  ggplot(spp, aes(x = as.factor(TREAT), y = EB, fill = SPP)) + 
    geom_boxplot() + 
    labs(x = "Number of Fires")
  
  