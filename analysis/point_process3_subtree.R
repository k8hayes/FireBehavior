# "subtree" fuel category - trees under DBH
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(here)

subtree <- read.csv(here("data/herb.csv"))

# filtering out everything below DBH
subtree <- subtree %>%
  filter(HEIGHT_M <1.37) %>%
  rename("BD" = "DBH_CM")

unique(subtree$SPP)

# adding expansion factor
subtree$m2 <- 9 # default is measuring 3 x 3 square meters

subtree$m2[subtree$PLOT == "15_3"] <- 25
subtree$m2[subtree$PLOT == "17_3"] <- 25
subtree$m2[subtree$PLOT == "32_2"] <- 100
subtree$m2[subtree$PLOT == "52_1"] <- 100
subtree$m2[subtree$PLOT == "40_2"] <- 100
subtree$m2[subtree$PLOT == "55_3"] <- 100
subtree$m2[subtree$PLOT == "10_0"] <- 100
subtree$m2[subtree$PLOT == "33_1"] <- 100

# Density ##################################################
  # mean number of subtrees per plot
    meantree3 <- subtree %>%
      filter(TREAT == 3) %>%
      group_by(PLOT) %>%
      summarise(n = n(), m2 = unique(m2))

# scaling to landscape level
  meantree3$n_m2 <- round(meantree3$n / meantree3$m2) 

  meantree3$n_plot <- round(meantree3$n_m2 * 400)
  
  round(mean(meantree3$n_plot)) # 3323
  
  meantree3$n_landscape <- round(meantree3$n_m2 * 2400) #19938
  
  #  generating point cloud  
  x <- round(runif(round(mean(meantree3$n_landscape)), 0, 800))
  y <- round(runif(round(mean(meantree3$n_landscape)), 0, 300))
  
   system.time(plot(x,y))
  
  system.time(loc <- cbind(x,y))
  system.time(loc <- as.data.frame(loc))
  
  ## Marks ####################
  
  ### Species ###############################
  # average number per species within plot
  meantreespp3 <- subtree %>% 
    filter(TREAT == 3) %>%
    group_by(PLOT, SPP) %>% # group by plot / spp
    summarise(n = n()) %>% # counts number of spp within plot
    group_by(SPP) %>% # groups by spp only
    summarise(AV = round(mean(n))) # average of count
  meantreespp3$prob <- meantreespp3$AV / sum(meantreespp3$AV)# can't use 119 # doesn't add to 1
  sum(meantreespp3$prob) # checking if it adds to 1
  
  loc$SPP <- simple_ra(N = length(loc$y), 
                       prob_each = meantreespp3$prob,
                       conditions = meantreespp3$SPP) # names
  
  rm(meantreespp3)
  
  ### Live / Dead #############################
  subtree$LIVE_DEAD <- NA
  subtree$LIVE_DEAD[subtree$CANOPY > 0] <- "LIVE"
  subtree$LIVE_DEAD[subtree$CANOPY == 0] <- "DEAD"
  
  meanCan3 <- subtree %>% 
    filter(TREAT == 3) %>% 
    filter(CANOPY != "NA") %>%
    group_by(PLOT, SPP, LIVE_DEAD) %>% # group by plot / spp
    summarise(n = n()) %>% # counts number of spp within plot
    complete(expand(subtree, LIVE_DEAD), fill = list(n = 0 )) %>%
    filter(LIVE_DEAD != "NA") %>%
    pivot_wider(names_from = LIVE_DEAD, values_from = n)
  
  meanCan3$tot <- meanCan3$LIVE + meanCan3$DEAD
  
  meanCan3$prob <- NA
  meanCan3$prob <- round(meanCan3$LIVE / meanCan3$tot, digits = 2)
  
  meanCan3 <- meanCan3 %>%
    group_by(SPP) %>%
    summarise(mean(prob))
  
  pime_loc <- loc %>% 
    filter(SPP == "PIME") 
  pime_loc$LIVE_DEAD <- simple_ra(N = nrow(pime_loc),
                                  prob = meanCan3$prob[meanCan3$SPP == "PIME"],
                                  conditions = c(1,0))
  bene_loc <- loc %>% 
    filter(SPP == "BENE") 
  bene_loc$LIVE_DEAD <- simple_ra(N = nrow(bene_loc),
                                  prob_each = meanCan3$prob[meanCan3$SPP == "BENE"],
                                  conditions = c(1,0))
  potr_loc <- loc %>% 
    filter(SPP == "POTR") 
  potr_loc$LIVE_DEAD <- simple_ra(N = nrow(potr_loc),
                                  prob_each = meanCan3$prob[meanCan3$SPP == "POTR"],
                                  conditions = c(1,0))
  alcr_loc <- loc %>% 
    filter(SPP == "ALCR") 
  alcr_loc$LIVE_DEAD <- simple_ra(N = nrow(alcr_loc),
                                  prob_each = meanCan3$prob[meanCan3$SPP == "ALCR"],
                                  conditions = c(1,0))
  salix_loc <- loc %>% 
    filter(SPP == "SALIX") 
  salix_loc$LIVE_DEAD <- simple_ra(N = nrow(salix_loc),
                                   prob_each = meanCan3$prob[meanCan3$SPP == "SALIX"],
                                   conditions = c(1,0))
  # arcto_loc <- loc %>% 
  # filter(SPP == "ARCTO") 
  # arcto_loc$LIVE_DEAD <- simple_ra(N = nrow(arcto_loc),
  # prob_each = c(meanCan3$prob[meanCan3$SPP == "ARCTO"],
  #               1 - meanCan3$prob[meanCan3$SPP == "ARCTO"]),
  # conditions = c(1,0))
  loc <- rbind(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc)
  
  rm(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc,
     meanCan3)
  
  ### Height ##################################   
  # what's the average height per species within 1x burns
  height <- subtree %>% 
    filter(TREAT == 3) %>%
    filter(HEIGHT_M != "NA") %>% 
    filter(SPP != "PIGL") %>%
    group_by(SPP, LIVE_DEAD) %>%
    summarise(AV = mean(HEIGHT_M), SD = sd(HEIGHT_M)) %>%
    ungroup() %>%
    complete(SPP,LIVE_DEAD) %>%
    filter(LIVE_DEAD != "NA")
  
  # No dead alcr or aspen in these plots, so they don't have heights
  # assigning the same mean and sd as the live trees
  
  height$AV[3] <- height$AV[4]
  height$SD[3] <- height$SD[4] # arcto 
  
  height$AV[9] <- height$AV[10] # poba
  height$SD[9] <- height$SD[10]
  
  height$AV[7] <- height$AV[8] # poba
  height$SD[7] <- height$SD[8]
  
  #### Assigning heights ################
  # initializing column
  loc$HEIGHT_M <- 0
  
  ##### Birch 
  bene_loc <- loc %>%
    filter(SPP == "BENE")
  bene_height <- height %>%
    filter(SPP == "BENE")
  bene_loc$HEIGHT_M[bene_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = bene_height$AV[bene_height$LIVE_DEAD=="DEAD"],
                                                          sd = bene_height$SD[bene_height$LIVE_DEAD=="DEAD"])
  bene_loc$HEIGHT_M[bene_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = bene_height$AV[bene_height$LIVE_DEAD=="LIVE"],
                                                          sd = bene_height$SD[bene_height$LIVE_DEAD=="LIVE"])
  ##### Aspen
  potr_loc <- loc %>%
    filter(SPP == "POTR")
  potr_height <- height %>%
    filter(SPP == "POTR")
  potr_loc$HEIGHT_M[potr_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = potr_height$AV[potr_height$LIVE_DEAD=="DEAD"],
                                                          sd = potr_height$SD[potr_height$LIVE_DEAD=="DEAD"])
  potr_loc$HEIGHT_M[potr_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = potr_height$AV[potr_height$LIVE_DEAD=="LIVE"],
                                                          sd = potr_height$SD[potr_height$LIVE_DEAD=="LIVE"])
  ##### Spruce
  pime_loc <- loc %>%
    filter(SPP == "PIME")
  pime_height <- height %>%
    filter(SPP == "PIME")
  pime_loc$HEIGHT_M[pime_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = pime_height$AV[pime_height$LIVE_DEAD=="DEAD"],
                                                          sd = pime_height$SD[pime_height$LIVE_DEAD=="DEAD"])
  pime_loc$HEIGHT_M[pime_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = pime_height$AV[pime_height$LIVE_DEAD=="LIVE"],
                                                          sd = pime_height$SD[pime_height$LIVE_DEAD=="LIVE"])
  ##### Alder 
  alcr_loc <- loc %>%
    filter(SPP == "ALCR")
  alcr_height <- height %>%
    filter(SPP == "ALCR")
  alcr_loc$HEIGHT_M[alcr_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = alcr_height$AV[alcr_height$LIVE_DEAD=="DEAD"],
                                                          sd = alcr_height$SD[alcr_height$LIVE_DEAD=="DEAD"])
  alcr_loc$HEIGHT_M[alcr_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = alcr_height$AV[alcr_height$LIVE_DEAD=="LIVE"],
                                                          sd = alcr_height$SD[alcr_height$LIVE_DEAD=="LIVE"])
  
  # loc$HEIGHT_M[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree3$n)), a = 0,mean = height$AV[height$SPP == "ARCTO"],
  #       sd = height$SD[height$SPP == "ARCTO"])
  ##### Willow 
  salix_loc <- loc %>%
    filter(SPP == "SALIX")
  salix_height <- height %>%
    filter(SPP == "SALIX")
  salix_loc$HEIGHT_M[salix_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                            a = 0, mean = salix_height$AV[salix_height$LIVE_DEAD=="DEAD"],
                                                            sd = salix_height$SD[salix_height$LIVE_DEAD=="DEAD"])
  salix_loc$HEIGHT_M[salix_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                            a = 0, mean = salix_height$AV[salix_height$LIVE_DEAD=="LIVE"],
                                                            sd = salix_height$SD[salix_height$LIVE_DEAD=="LIVE"])
  
  loc <- rbind(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc)
  
  rm(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc,
     height)
  rm(bene_height, potr_height, pime_height, salix_height, alcr_height)
  
  loc$HEIGHT_M <- round(loc$HEIGHT_M, digits = 2)
  hist(loc$HEIGHT_M)
  
  ### Biomass #########################
  #### assigning BDs ##################################
  sub_assign <- subtree %>%
    filter(TREAT == 3) %>%
    filter(!is.na(LIVE_DEAD)) %>%
    filter(SPP != "PIGL") %>%
    group_by(SPP, LIVE_DEAD) %>%
    summarise(AV = mean(BD), SD = sd(BD)) %>%
    ungroup() %>%
    complete(SPP,LIVE_DEAD) %>%
    filter(LIVE_DEAD != "NA")
  
  sub_assign$AV[7] <- sub_assign$AV[8]
  sub_assign$SD[7] <- sub_assign$SD[8]
  
  sub_assign$AV[3] <- sub_assign$AV[4]
  sub_assign$SD[3] <- sub_assign$SD[4]
  
  sub_assign$AV[9] <- sub_assign$AV[10]
  sub_assign$SD[9] <- sub_assign$SD[10]
  
  sub_assign$AV[12] <- sub_assign$AV[11]
  sub_assign$SD[12] <- sub_assign$SD[11]
  
  ##### Birch 
  bene_loc <- loc %>%
    filter(SPP == "BENE")
  bene_dbh <- sub_assign %>%
    filter(SPP == "BENE")
  bene_loc$BD[bene_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                        a = 0, mean = bene_dbh$AV[bene_dbh$LIVE_DEAD=="DEAD"],
                                                        sd = bene_dbh$SD[bene_dbh$LIVE_DEAD=="DEAD"])
  bene_loc$BD[bene_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                        a = 0, mean = bene_dbh$AV[bene_dbh$LIVE_DEAD=="LIVE"],
                                                        sd = bene_dbh$SD[bene_dbh$LIVE_DEAD=="LIVE"])
  ##### Aspen 
  potr_loc <- loc %>%
    filter(SPP == "POTR")
  potr_dbh <- sub_assign %>%
    filter(SPP == "POTR")
  potr_loc$BD[potr_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                        a = 0, mean = potr_dbh$AV[potr_dbh$LIVE_DEAD=="DEAD"],
                                                        sd = potr_dbh$SD[potr_dbh$LIVE_DEAD=="DEAD"])
  potr_loc$BD[potr_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                        a = 0, mean = potr_dbh$AV[potr_dbh$LIVE_DEAD=="LIVE"],
                                                        sd = potr_dbh$SD[potr_dbh$LIVE_DEAD=="LIVE"])
  
  ##### Spruce 
  pime_loc <- loc %>%
    filter(SPP == "PIME")
  pime_dbh <- sub_assign %>%
    filter(SPP == "PIME")
  pime_loc$BD[pime_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                        a = 0, mean = pime_dbh$AV[pime_dbh$LIVE_DEAD=="DEAD"],
                                                        sd = pime_dbh$SD[pime_dbh$LIVE_DEAD=="DEAD"])
  pime_loc$BD[pime_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                        a = 0, mean = pime_dbh$AV[pime_dbh$LIVE_DEAD=="LIVE"],
                                                        sd = pime_dbh$SD[pime_dbh$LIVE_DEAD=="LIVE"])
  ##### Alder 
  alcr_loc <- loc %>%
    filter(SPP == "ALCR")
  alcr_dbh <- sub_assign %>%
    filter(SPP == "ALCR")
  alcr_loc$BD[alcr_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                        a = 0, mean = alcr_dbh$AV[alcr_dbh$LIVE_DEAD=="DEAD"],
                                                        sd = alcr_dbh$SD[alcr_dbh$LIVE_DEAD=="DEAD"])
  alcr_loc$BD[alcr_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                        a = 0, mean = alcr_dbh$AV[alcr_dbh$LIVE_DEAD=="LIVE"],
                                                        sd = alcr_dbh$SD[alcr_dbh$LIVE_DEAD=="LIVE"])
  
  # loc$BD[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree3$n)), a = 0,mean = dbh_assign$AV[dbh_assign$SPP == "ARCTO"],
  #       sd = dbh_assign$SD[dbh_assign$SPP == "ARCTO"])
  
  ##### Willow 
  salix_loc <- loc %>%
    filter(SPP == "SALIX")
  salix_dbh <- sub_assign %>%
    filter(SPP == "SALIX")
  salix_loc$BD[salix_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = salix_dbh$AV[salix_dbh$LIVE_DEAD=="DEAD"],
                                                          sd = salix_dbh$SD[salix_dbh$LIVE_DEAD=="DEAD"])
  salix_loc$BD[salix_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree3$n_landscape)), 
                                                          a = 0, mean = salix_dbh$AV[salix_dbh$LIVE_DEAD=="LIVE"],
                                                          sd = salix_dbh$SD[salix_dbh$LIVE_DEAD=="LIVE"])
  
  loc <- rbind(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc)
  
  rm(pime_dbh, potr_dbh, alcr_dbh, salix_dbh, bene_dbh)
  rm(pime_loc, potr_loc, alcr_loc, salix_loc, bene_loc,
     sub_assign)
  
  #### Stem ############################### # ONLY WANT FOLIAGE FOR SUBTREE
  # loc$STEM_BIOMASS <- 0
  # loc$STEM_BIOMASS[loc$SPP == "PIME"] <-  117.91*(loc$DBH[loc$SPP == "PIME"]^1.99) # Alexander et al. 2012
  # loc$STEM_BIOMASS[loc$SPP == "POTR"] <-  64.01*(loc$DBH[loc$SPP == "POTR"]^2.51) # Alexander et al. 2012
  # loc$STEM_BIOMASS[loc$SPP == "BENE"] <-  147.96*(loc$DBH[loc$SPP == "BENE"]^2.25) # Alexander et al. 2012
  # loc$STEM_BIOMASS[loc$SPP == "ALCR"] <-  exp(4.5 + 2.3*log(loc$DBH[loc$SPP == "ALCR"])) # Binkley et al. 1984
  # loc$STEM_BIOMASS[loc$SPP == "SALIX"] <- 10^(2.167 + 1.03*log10(loc$DBH[loc$SPP == "SALIX"])) # Bond-Lamberty et al. 2002
  
  # biomass_stem <- dbh %>% 
  #   filter(TREAT == 1) %>%
  #   group_by(SPP) %>%
  #   summarise(AV = mean(STEM_BIOMASS), SD = sd(STEM_BIOMASS)) 
  # 
  # loc$STEM_BIOMASS[loc$SPP=="BENE"] <- rtruncnorm(round(mean(meantree3$n_landscape)),a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "BENE"],
  #                                           sd = biomass_stem$SD[biomass_stem$SPP == "BENE"])
  # loc$STEM_BIOMASS[loc$SPP=="POTR"] <- rtruncnorm(round(mean(meantree3$n_landscape)), a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "POTR"],
  #                                           sd = biomass_stem$SD[biomass_stem$SPP == "POTR"])
  # loc$STEM_BIOMASS[loc$SPP=="PIME"] <- rtruncnorm(round(mean(meantree3$n_landscape)), a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "PIME"],
  #                                           sd = biomass_stem$SD[biomass_stem$SPP == "PIME"])
  # loc$STEM_BIOMASS[loc$SPP=="ALCR"] <- rtruncnorm(round(mean(meantree3$n_landscape)), a =0, mean = biomass_stem$AV[biomass_stem$SPP == "ALCR"],
  #                                           sd = biomass_stem$SD[biomass_stem$SPP == "ALCR"])
  # #loc$STEM_BIOMASS[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree3$n)), a = 0,mean = biomass_stem$AV[biomass_stem$SPP == "ARCTO"],
  #                                           # sd = biomass_stem$SD[biomass_stem$SPP == "ARCTO"])
  # loc$STEM_BIOMASS[loc$SPP=="SALIX"] <- rtruncnorm(round(mean(meantree3$n_landscape)), a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "SALIX"],
  #                                            sd = biomass_stem$SD[biomass_stem$SPP == "SALIX"])
  # #### Foliage ####################
  loc$FOL_BIOMASS <- 0
  loc$FOL_BIOMASS[loc$SPP == "PIME"] <-  55.4*(loc$BD[loc$SPP == "PIME"]^1.47) # Alexander et al. 2012
  loc$FOL_BIOMASS[loc$SPP == "POTR"] <-  18.98*(loc$BD[loc$SPP == "POTR"]^1.53) # Alexander et al. 2012
  loc$FOL_BIOMASS[loc$SPP == "BENE"] <-  6.39*(loc$BD[loc$SPP == "BENE"]^2.1) # Alexander et al. 2012
  loc$FOL_BIOMASS[loc$SPP == "ALCR"] <-  10^(1.82 + 2.38*log10(loc$BD[loc$SPP == "ALCR"])) # Binkley et al. 1984
  loc$FOL_BIOMASS[loc$SPP == "SALIX"] <- 10^(2.023 + 1.065*log10(loc$BD[loc$SPP == "SALIX"])) # Bond-Lamberty et al. 2002
  
  # biomass_fol <- dbh %>% 
  #   filter(TREAT == 1) %>%
  #   group_by(SPP) %>%
  #   summarise(AV = mean(FOL_BIOMASS), SD = sd(FOL_BIOMASS)) 
  # 
  # loc$FOL_BIOMASS[loc$SPP=="BENE"] <- rtruncnorm(round(mean(meantree3$n_landscape)),a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "BENE"],
  #                                                 sd = biomass_fol$SD[biomass_fol$SPP == "BENE"])
  # loc$FOL_BIOMASS[loc$SPP=="POTR"] <- rtruncnorm(round(mean(meantree3$n_landscape)), a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "POTR"],
  #                                                 sd = biomass_fol$SD[biomass_fol$SPP == "POTR"])
  # loc$FOL_BIOMASS[loc$SPP=="PIME"] <- rtruncnorm(round(mean(meantree3$n_landscape)), a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "PIME"],
  #                                                 sd = biomass_fol$SD[biomass_fol$SPP == "PIME"])
  # loc$FOL_BIOMASS[loc$SPP=="ALCR"] <- rtruncnorm(round(mean(meantree3$n_landscape)), a =0, mean = biomass_fol$AV[biomass_fol$SPP == "ALCR"],
  #                                                 sd = biomass_fol$SD[biomass_fol$SPP == "ALCR"])
  # #loc$FOL_BIOMASS[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree3$n)), a = 0,mean = biomass_fol$AV[biomass_fol$SPP == "ARCTO"],
  # # sd = biomass_fol$SD[biomass_fol$SPP == "ARCTO"])
  # loc$FOL_BIOMASS[loc$SPP=="SALIX"] <- rtruncnorm(round(mean(meantree3$n_landscape)), a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "SALIX"],
  #                                                  sd = biomass_fol$SD[biomass_fol$SPP == "SALIX"])
  # 
  # rm(biomass_fol, biomass_stem)  
  # setting dead trees to have foliar biomass = 0
  loc$FOL_BIOMASS[loc$LIVE_DEAD ==0] <- 0
  # 
  
  ### Crown Width #####################################
  
  loc$CROWN_WIDTH <- 2*(sqrt(loc$FOL_BIOMASS / pi * loc$HEIGHT))
  
  loc$CROWN_WIDTH[loc$LIVE_DEAD == 0] <- 0
  
  ### Crown Base Height ######################################
  ## assuming 0 for now
  
  loc$CROWN_BASE_HEIGHT <- 0
  
  ## Exporting ##############################################
  
  # tree_input <- loc[sample(nrow(loc), 100),]
  # 
  # write.csv(tree_input, here("data/output/tree_input100.csv"), row.names = F)
  # 
  # rm(tree_input)
  
  #Assume that our 'area of interest' for collecting fire behavior data is in a subset of the 1000 m x 400 m domain
  # AOI: x{400,600}, y{100,300}
  # Each tree will also need a unique identifier
  # NOT tracking dead trees
  
  # dbh from cm to m
  loc$DBH_M <- NA
  loc$DBH_M <- loc$BD / 100
  
  loc$CROWN_WIDTH_M <-loc$CROWN_WIDTH * 100
  
  trees <- mutate(loc,
                  export = if_else((x >= 525 & x <= 600 & y >= 125 & y<=200 & LIVE_DEAD == 1),
                                   TRUE, FALSE)) %>%
    group_by(export) %>%
    mutate(id =  row_number())
  
  length(which(trees$export == "TRUE")) # 4340# can do 4096 total
  
  ggplot(trees, aes(x,y,col=export)) + geom_point() + coord_equal()
  
  #Make tree list ready for WFDS
  
  
  
  trees_WFDS = mutate(trees,
                      wfds_canopy = if_else(LIVE_DEAD==0, "", 
                                            paste0("&TREE XYZ=", round(x,1), ",", round(y,1),
                                                   ",0,PART_ID='TREE',FUEL_GEOM='CYLINDER',CROWN_WIDTH=", round(CROWN_WIDTH_M,1),
                                                   ",CROWN_BASE_HEIGHT=", round(CROWN_BASE_HEIGHT,1),
                                                   ",TREE_HEIGHT=", round(HEIGHT_M,1),
                                                   ",OUTPUT_TREE=.", export,
                                                   ".,LABEL='", paste0('tree',id), "' /")),
                      wfds_stem = if_else(CROWN_BASE_HEIGHT==0, '', 
                                          paste0("&TREE XYZ=",round(x,1), ",", round(y,1),
                                                 ",0,PART_ID='TRUNK',FUEL_GEOM='CYLINDER',CROWN_WIDTH=",round(DBH_M/100,1),
                                                 ",CROWN_BASE_HEIGHT=0",
                                                 ",TREE_HEIGHT=",CROWN_BASE_HEIGHT,"/"
                                          ))
  )
  
  write.csv(trees_WFDS, here("data/output/subtree_WFDS_3x.csv"), row.names = FALSE)
  
  #Note that all trees get labelled but only those trees in the area of interest (where export == TRUE)
  # will actually have their heating and mass tracked
  #Also, length units are in meters so dbh, commonly measured in cm, are expressed in m.
  
  