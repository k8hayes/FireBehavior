# Point process models
# unburn

# Set up ##########################
# !diagnostics off
library(tidyverse)
library(truncnorm)
library(here)
library(randomizr) # https://cran.r-project.org/web/packages/randomizr/vignettes/randomizr_vignette.html

dbh <- read.csv(here("data/dbh_clumpsas1_scale.csv"))

# taking ARCTO out for now
dbh <- dbh %>%
  filter(SPP != "ARCTO") %>%
  filter(CANOPY != "NA")

dbh0 <- dbh %>%
  filter(TREAT == 0)
summary(dbh0$DBH_CM)

# 1x burn ######################################
## Points #########################
  # mean number of trees per plot
    meantree0 <- dbh %>%
      filter(TREAT == 0) %>%
      group_by(PLOT) %>%
      summarise(n = n(), QUAD = unique(QUAD), EXP_FACT = unique(EXP_FACT))
    round(mean(meantree0$n)) # 105
    
    # scale up to 1000 * 400
    # meantree1$n_landscape <- meantree1$n * meantree1$EXP_FACT
    
    # Scale to 800 x 300
    meantree0$EXP_FACT[meantree0$EXP_FACT == 4000] <- 2400
    
    meantree0$n_landscape <- meantree0$n * meantree0$EXP_FACT
    
    round(mean(meantree0$n_landscape)) # 252000
    
  # generating point cloud  
  x <- round(runif(round(mean(meantree0$n_landscape)), 0, 800))
  y <- round(runif(round(mean(meantree0$n_landscape)), 0, 300))

  # system.time(plot(x,y))

  system.time(loc <- cbind(x,y))
  system.time(loc <- as.data.frame(loc))

## Marks ####################

  ### Species ###############################
    # average number per species within plot
    meantreespp0 <- dbh %>% 
      filter(TREAT == 0) %>% # only 1x burns right now
      group_by(PLOT, SPP) %>% # group by plot / spp
      summarise(n = n()) %>% # counts number of spp within plot
        group_by(SPP) %>% # groups by spp only
        summarise(AV = round(mean(n))) # average of count
    meantreespp0$prob <- meantreespp0$AV / sum(meantreespp0$AV)# can't use 119 # doesn't add to 1
    sum(meantreespp0$prob) # checking if it adds to 1
    
   loc$SPP <- simple_ra(N = length(loc$y), 
              prob_each = meantreespp0$prob,
              conditions = meantreespp0$SPP) # names
   
   rm(meantreespp0)
 
 ### Live / Dead #############################
   dbh$LIVE_DEAD <- NA
   dbh$LIVE_DEAD[dbh$CANOPY > 0] <- "LIVE"
   dbh$LIVE_DEAD[dbh$CANOPY == 0] <- "DEAD"
   
   meanCan0 <- dbh %>% 
     filter(TREAT == 0) %>% 
     filter(CANOPY != "NA") %>%
     group_by(PLOT, SPP, LIVE_DEAD) %>% # group by plot / spp
     summarise(n = n()) %>% # counts number of spp within plot
     complete(expand(dbh, LIVE_DEAD), fill = list(n = 0 )) %>%
     filter(LIVE_DEAD != "NA") %>%
     pivot_wider(names_from = LIVE_DEAD, values_from = n)

   meanCan0$tot <- meanCan0$LIVE + meanCan0$DEAD
  
   meanCan0$prob <- NA ; meanCan0$prob <- round(meanCan0$LIVE / meanCan0$tot, digits = 2)
   
   meanCan0 <- meanCan0 %>%
     group_by(SPP) %>%
     summarise(mean(prob))
   
  pime_loc <- loc %>% 
    filter(SPP == "PIME") 
  pime_loc$LIVE_DEAD <- simple_ra(N = nrow(pime_loc),
                             prob = meanCan0$prob[meanCan0$SPP == "PIME"],
                            conditions = c(1,0))
   bene_loc <- loc %>% 
    filter(SPP == "BENE") 
  bene_loc$LIVE_DEAD <- simple_ra(N = nrow(bene_loc),
                                  prob_each = meanCan0$prob[meanCan0$SPP == "BENE"],
                                  conditions = c(1,0))
  
  salix_loc <- loc %>% 
    filter(SPP == "SALIX") 
  salix_loc$LIVE_DEAD <- simple_ra(N = nrow(salix_loc),
                                  prob_each = meanCan0$prob[meanCan0$SPP == "SALIX"],
                                  conditions = c(1,0))
  # arcto_loc <- loc %>% 
    # filter(SPP == "ARCTO") 
  # arcto_loc$LIVE_DEAD <- simple_ra(N = nrow(arcto_loc),
                                  # prob_each = c(meanCan1$prob[meanCan1$SPP == "ARCTO"],
                                  #               1 - meanCan1$prob[meanCan1$SPP == "ARCTO"]),
                                  # conditions = c(1,0))
  loc <- rbind(pime_loc, bene_loc,  salix_loc)
  
    rm(pime_loc, bene_loc,  salix_loc, 
       meanCan0)
  
  
    ### Height ##################################   
   # what's the average height per species within 1x burns
    height <- dbh %>% 
      filter(TREAT == 0) %>%
      filter(HEIGHT_M != "NA") %>% # one NA in PIME # need to check
      group_by(SPP, LIVE_DEAD) %>%
      summarise(AV = mean(HEIGHT_M), SD = sd(HEIGHT_M)) %>%
      complete(expand(dbh,LIVE_DEAD)) %>%
      filter(LIVE_DEAD != "NA")
      
  #### Assigning heights ################
      # initializing column
        loc$HEIGHT_M <- 0
    
  ##### Birch 
  bene_loc <- loc %>%
    filter(SPP == "BENE")
  bene_height <- height %>%
    filter(SPP == "BENE")
  bene_loc$HEIGHT_M[bene_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                              a = 0, mean = bene_height$AV[bene_height$LIVE_DEAD=="DEAD"],
                                                              sd = bene_height$SD[bene_height$LIVE_DEAD=="DEAD"])
  bene_loc$HEIGHT_M[bene_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                              a = 0, mean = bene_height$AV[bene_height$LIVE_DEAD=="LIVE"],
                                                              sd = bene_height$SD[bene_height$LIVE_DEAD=="LIVE"])
  ##### Spruce
  pime_loc <- loc %>%
    filter(SPP == "PIME")
  pime_height <- height %>%
    filter(SPP == "PIME")
  pime_loc$HEIGHT_M[pime_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                          a = 0, mean = pime_height$AV[pime_height$LIVE_DEAD=="DEAD"],
                                                          sd = pime_height$SD[pime_height$LIVE_DEAD=="DEAD"])
  pime_loc$HEIGHT_M[pime_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                          a = 0, mean = pime_height$AV[pime_height$LIVE_DEAD=="LIVE"],
                                                          sd = pime_height$SD[pime_height$LIVE_DEAD=="LIVE"])
##### Willow 
  salix_loc <- loc %>%
    filter(SPP == "SALIX")
  salix_height <- height %>%
    filter(SPP == "SALIX")
  salix_loc$HEIGHT_M[salix_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                          a = 0, mean = salix_height$AV[salix_height$LIVE_DEAD=="DEAD"],
                                                          sd = salix_height$SD[salix_height$LIVE_DEAD=="DEAD"])
  salix_loc$HEIGHT_M[salix_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                          a = 0, mean = salix_height$AV[salix_height$LIVE_DEAD=="LIVE"],
                                                          sd = salix_height$SD[salix_height$LIVE_DEAD=="LIVE"])

  loc <- rbind(pime_loc, bene_loc,  salix_loc)
  
  rm(pime_loc, bene_loc,  salix_loc, 
     height)
  rm(bene_height,  pime_height, salix_height)
  
  loc$HEIGHT_M <- round(loc$HEIGHT_M, digits = 2)
  hist(loc$HEIGHT_M)
 
  # checking 90th percentile of live heights
  live <- loc %>%
    filter(LIVE_DEAD == 1)
  quantile(live$HEIGHT_M, .9) # 1.68 meters
  
  rm(live)

### Biomass #########################
  #### assigning DBHs ##################################
  dbh_assign <- dbh %>%
    filter(TREAT == 0) %>%
    filter(!is.na(LIVE_DEAD)) %>%
    group_by(SPP, LIVE_DEAD) %>%
    filter(!is.na(DBH_CM)) %>%
    summarise(AV = mean(DBH_CM), SD = sd(DBH_CM)) %>%
    complete(expand(dbh,LIVE_DEAD)) %>%
    filter(LIVE_DEAD != "NA")

  ##### Birch 
  bene_loc <- loc %>%
    filter(SPP == "BENE")
  bene_dbh <- dbh_assign %>%
    filter(SPP == "BENE")
  bene_loc$DBH_CM[bene_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                          a = 0, mean = bene_dbh$AV[bene_dbh$LIVE_DEAD=="DEAD"],
                                                          sd = bene_dbh$SD[bene_dbh$LIVE_DEAD=="DEAD"])
  bene_loc$DBH_CM[bene_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                          a = 0, mean = bene_dbh$AV[bene_dbh$LIVE_DEAD=="LIVE"],
                                                          sd = bene_dbh$SD[bene_dbh$LIVE_DEAD=="LIVE"])
  ##### Spruce 
  pime_loc <- loc %>%
    filter(SPP == "PIME")
  pime_dbh <- dbh_assign %>%
    filter(SPP == "PIME")
  pime_loc$DBH_CM[pime_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                          a = 0, mean = pime_dbh$AV[pime_dbh$LIVE_DEAD=="DEAD"],
                                                          sd = pime_dbh$SD[pime_dbh$LIVE_DEAD=="DEAD"])
  pime_loc$DBH_CM[pime_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                          a = 0, mean = pime_dbh$AV[pime_dbh$LIVE_DEAD=="LIVE"],
                                                          sd = pime_dbh$SD[pime_dbh$LIVE_DEAD=="LIVE"])

  
  ##### Willow 
  salix_loc <- loc %>%
    filter(SPP == "SALIX")
  salix_dbh <- dbh_assign %>%
    filter(SPP == "SALIX")
  salix_loc$DBH_CM[salix_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                            a = 0, mean = salix_dbh$AV[salix_dbh$LIVE_DEAD=="DEAD"],
                                                            sd = salix_dbh$SD[salix_dbh$LIVE_DEAD=="DEAD"])
  salix_loc$DBH_CM[salix_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree0$n_landscape)), 
                                                            a = 0, mean = salix_dbh$AV[salix_dbh$LIVE_DEAD=="LIVE"],
                                                            sd = salix_dbh$SD[salix_dbh$LIVE_DEAD=="LIVE"])
  
  loc <- rbind(pime_loc, bene_loc,  salix_loc)
  
  rm(pime_dbh, potr_dbh, alcr_dbh, salix_dbh, bene_dbh)
  rm(pime_loc, potr_loc, alcr_loc, salix_loc, bene_loc,
     dbh_assign)
  
  hist(loc$DBH_CM)
  summary(loc$DBH_CM)
  
  #### Stem ###############################
  loc$STEM_BIOMASS <- 0
  loc$STEM_BIOMASS[loc$SPP == "PIME"] <-  117.91*(loc$DBH[loc$SPP == "PIME"]^1.99) # Alexander et al. 2012

  loc$STEM_BIOMASS[loc$SPP == "BENE"] <-  147.96*(loc$DBH[loc$SPP == "BENE"]^2.25) # Alexander et al. 2012
  
  loc$STEM_BIOMASS[loc$SPP == "SALIX"] <- 10^(2.167 + 1.03*log10(loc$DBH[loc$SPP == "SALIX"])) # Bond-Lamberty et al. 2002
  
  # biomass_stem <- dbh %>% 
  #   filter(TREAT == 1) %>%
  #   group_by(SPP) %>%
  #   summarise(AV = mean(STEM_BIOMASS), SD = sd(STEM_BIOMASS)) 
  # 
  # loc$STEM_BIOMASS[loc$SPP=="BENE"] <- rtruncnorm(round(mean(meantree1$n_landscape)),a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "BENE"],
  #                                           sd = biomass_stem$SD[biomass_stem$SPP == "BENE"])
  # loc$STEM_BIOMASS[loc$SPP=="POTR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "POTR"],
  #                                           sd = biomass_stem$SD[biomass_stem$SPP == "POTR"])
  # loc$STEM_BIOMASS[loc$SPP=="PIME"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "PIME"],
  #                                           sd = biomass_stem$SD[biomass_stem$SPP == "PIME"])
  # loc$STEM_BIOMASS[loc$SPP=="ALCR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a =0, mean = biomass_stem$AV[biomass_stem$SPP == "ALCR"],
  #                                           sd = biomass_stem$SD[biomass_stem$SPP == "ALCR"])
  # #loc$STEM_BIOMASS[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree1$n)), a = 0,mean = biomass_stem$AV[biomass_stem$SPP == "ARCTO"],
  #                                           # sd = biomass_stem$SD[biomass_stem$SPP == "ARCTO"])
  # loc$STEM_BIOMASS[loc$SPP=="SALIX"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "SALIX"],
  #                                            sd = biomass_stem$SD[biomass_stem$SPP == "SALIX"])
  # #### Foliage ####################
  loc$FOL_BIOMASS <- 0
  loc$FOL_BIOMASS[loc$SPP == "PIME"] <-  55.4*(loc$DBH[loc$SPP == "PIME"]^1.47) # Alexander et al. 2012
  loc$FOL_BIOMASS[loc$SPP == "BENE"] <-  6.39*(loc$DBH[loc$SPP == "BENE"]^2.1) # Alexander et al. 2012
  loc$FOL_BIOMASS[loc$SPP == "SALIX"] <- 10^(2.023 + 1.065*log10(loc$DBH[loc$SPP == "SALIX"])) # Bond-Lamberty et al. 2002
   
  # biomass_fol <- dbh %>% 
  #   filter(TREAT == 1) %>%
  #   group_by(SPP) %>%
  #   summarise(AV = mean(FOL_BIOMASS), SD = sd(FOL_BIOMASS)) 
  # 
  # loc$FOL_BIOMASS[loc$SPP=="BENE"] <- rtruncnorm(round(mean(meantree1$n_landscape)),a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "BENE"],
  #                                                 sd = biomass_fol$SD[biomass_fol$SPP == "BENE"])
  # loc$FOL_BIOMASS[loc$SPP=="POTR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "POTR"],
  #                                                 sd = biomass_fol$SD[biomass_fol$SPP == "POTR"])
  # loc$FOL_BIOMASS[loc$SPP=="PIME"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "PIME"],
  #                                                 sd = biomass_fol$SD[biomass_fol$SPP == "PIME"])
  # loc$FOL_BIOMASS[loc$SPP=="ALCR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a =0, mean = biomass_fol$AV[biomass_fol$SPP == "ALCR"],
  #                                                 sd = biomass_fol$SD[biomass_fol$SPP == "ALCR"])
  # #loc$FOL_BIOMASS[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree1$n)), a = 0,mean = biomass_fol$AV[biomass_fol$SPP == "ARCTO"],
  # # sd = biomass_fol$SD[biomass_fol$SPP == "ARCTO"])
  # loc$FOL_BIOMASS[loc$SPP=="SALIX"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "SALIX"],
  #                                                  sd = biomass_fol$SD[biomass_fol$SPP == "SALIX"])
  # 
  # rm(biomass_fol, biomass_stem)  
   # setting dead trees to have foliar biomass = 0
   loc$FOL_BIOMASS[loc$LIVE_DEAD ==0] <- 0
  # 

  ### Crown Width #####################################
  
  loc$CROWN_WIDTH <- 2*(sqrt(loc$STEM_BIOMASS + loc$FOL_BIOMASS / pi * loc$HEIGHT))
   
   loc$CROWN_WIDTH[loc$LIVE_DEAD == 0] <- 0

  ### Crown Base Height ######################################
   ## assuming 0 for now
  
   loc$CROWN_BASE_HEIGHT <- 0

  ## Exporting ##############################################
  
   tree_input <- loc[sample(nrow(loc), 100),]
   
   write.csv(tree_input, here("data/output/tree_input100_0.csv"), row.names = F)
   
   rm(tree_input)
   
  #Assume that our 'area of interest' for collecting fire behavior data is in a subset of the 1000 m x 400 m domain
  # AOI: x{400,600}, y{100,300}
  # Each tree will also need a unique identifier
  # NOT tracking dead trees
  
    # dbh from cm to m
   loc$DBH_M <- NA
   loc$DBH_M <- loc$DBH_CM / 100
   
   loc$CROWN_WIDTH_M <-loc$CROWN_WIDTH * 100
   
  trees <- mutate(loc,
                 export = if_else((x >= 525 & x <= 600 & y >= 125 & y<=200 & LIVE_DEAD == 1),
                                  TRUE, FALSE)) %>%
    group_by(export) %>%
    mutate(id =  row_number())
  
  length(which(trees$export == "TRUE")) # 3081 # can do 4096 total
  
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
  
  write.csv(trees_WFDS, here("data/output/tree_WFDS_0.csv"), row.names = FALSE)
  
  #Note that all trees get labelled but only those trees in the area of interest (where export == TRUE)
  # will actually have their heating and mass tracked
  #Also, length units are in meters so dbh, commonly measured in cm, are expressed in m.

