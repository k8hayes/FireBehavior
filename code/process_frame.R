# processing frames data

# Start ##################################
library(here)
library(tidyverse)

# combining dalton/steese
# dalt <- read.csv(here("data/frame/Dalt_frame.csv"))
# stee <- read.csv(here("data/frame/Steese_frame_2021.csv"))
# 
# colnames(dalt)
# colnames(stee)
# 
# dalt$Gram_POPR <- 0
# dalt$Tree_ARCTO <- 0
# dalt$Seed_ARCTO <- 0
# dalt$Forb_LADR <- 0
# dalt$Forb_MYSO <- 0
# dalt$Forb_ROSE <- 0
# dalt$Forb_COCA <- 0
# 
# stee$Forb_MEPA <- 0
# stee$Forb_WHFL <- 0
# stee$Forb_ROCA <- 0
# stee$Forb <- 0
# stee$Gram <- 0
# 
# frame <- rbind(dalt, stee)
# rm(dalt, stee)
# 
# frame[is.na(frame)] <- 0
# 
# write.csv(frame, here("data/frame/frame.csv"), row.names = F)

frame <- read.csv(here("data/frame/frame.csv"))

# Pivot ########################
frame <- frame %>%
  dplyr::filter(Fires == 1 | Fires == 3) %>%
  pivot_longer(!c(Site, Frame, Height, Plot, Corner, Fires), names_to = "Cover",
               values_to = "presence")

# Group cover type #####################################
    frame$Cov_type[frame$Cover == "Gram"] <- "Gram"
    frame$Cov_type[frame$Cover == "Gram_POAA"] <- "Gram"
    frame$Cov_type[frame$Cover == "Gram_POPR"] <- "Gram"
  
    frame$Cov_type[frame$Cover == "Forb"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_CHAN"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_OXOX"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_ROCA"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_Cabbage"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_DAISY"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_VACC"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_CROW"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_MEPA"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_WHFL"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_LADR"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_MYSO"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_ROSE"] <- "Forb"
    frame$Cov_type[frame$Cover == "Forb_COCA"] <- "Forb"
  
    frame$Cov_type[frame$Cover == "Vasc_EQSC"] <- "Vasc"
    frame$Cov_type[frame$Cover == "Vasc_EQSY"] <- "Vasc"
    frame$Cov_type[frame$Cover == "Vasc_EQAR"] <- "Vasc"
    
    frame$Cov_type[frame$Cover == "Shrub_LEGR"] <- "Shrub"
  
    frame$Cov_type[frame$Cover == "Tree_POTR"] <- "Tree"
    frame$Cov_type[frame$Cover == "Tree_PIME"] <- "Tree"
    frame$Cov_type[frame$Cover == "Tree_BENE"] <- "Tree"
    frame$Cov_type[frame$Cover == "Tree_SALIX"] <- "Tree"
    frame$Cov_type[frame$Cover == "Tree_ALCR"] <- "Tree"
    frame$Cov_type[frame$Cover == "Tree_ARCTO"] <- "Tree"
    
    frame$Cov_type[frame$Cover == "Seed_POTR"] <- "Seed"
    frame$Cov_type[frame$Cover == "Seed_PIME"] <- "Seed"
    frame$Cov_type[frame$Cover == "Seed_BENE"] <- "Seed"
    frame$Cov_type[frame$Cover == "Seed_SALIX"] <- "Seed"
    frame$Cov_type[frame$Cover == "Seed_ALCR"] <- "Seed"
    frame$Cov_type[frame$Cover == "Seed_ARCTO"] <- "Seed"
    
    frame$Cov_type[frame$Cover == "X1_hour"] <- "CWD"
    frame$Cov_type[frame$Cover == "X10_hour"] <- "CWD"
    frame$Cov_type[frame$Cover == "X100_hour"] <- "CWD"
    frame$Cov_type[frame$Cover == "X1000_hour"] <- "CWD"
    
    frame <- frame %>%
      group_by(Site, Plot, Fires, Frame, Height, Corner, Cov_type) %>%
      summarise(presence = sum(presence))

# Reassign ##########################
    frame$presence[frame$Cov_type == "CWD" & frame$Height == "0.25-0"] <- 0
    frame$presence[frame$Cov_type == "Seed" & frame$Height == "0.25-0"] <- 0
    frame$presence[frame$Cov_type == "Gram" & frame$Height == "0.25-0"] <- 0
    frame$presence[frame$Cov_type == "Forb" & frame$Height == "0.25-0"] <- 0
    frame$presence[frame$Cov_type == "Vasc" & frame$Height == "0.25-0"] <- 0

frame_corner <- frame %>%
  group_by(Site, Plot, Fires, Frame, Corner) %>%
  summarise(presence = sum(presence))

frame_corner$count[frame_corner$presence == 0] <- 0
frame_corner$count[frame_corner$presence != 0] <- 1

frame_count <- frame_corner %>%
  group_by(Site, Plot, Fires, Frame) %>%
  summarise(presence = sum(count))

# Classifying frames with 2 corners bare as bare
frame_count$bare[frame_count$presence == 0] <- 0 # all corners bare
frame_count$bare[frame_count$presence == 1] <- 0 # one corner with fuel
frame_count$bare[frame_count$presence == 2] <- 0 # 2 corners bare
frame_count$bare[frame_count$presence == 3] <- 1 # 1 corner bare
frame_count$bare[frame_count$presence == 4] <- 1 # no corners bare

frame_tot <- frame_count %>%
  group_by(Site, Fires, Plot) %>%
  summarise(bare = sum(bare))

frame_count %>%
  filter(Site == "Dalton")

frame_tot$tot <- frame_tot$bare / 10

frame_tot %>%
  group_by(Fires) %>%
  summarise(AV = mean(tot), SD = sd(tot))

frame_tot %>%
  filter(Site == "Dalton")
