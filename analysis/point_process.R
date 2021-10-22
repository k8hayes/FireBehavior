# Point process models

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

# 1x burn ######################################
## Points #########################
  # mean number of trees per plot
    meantree1 <- dbh %>%
      filter(TREAT == 1) %>%
      group_by(PLOT) %>%
      summarise(n = n(), QUAD = unique(QUAD), EXP_FACT = unique(EXP_FACT))
    round(mean(meantree1$n)) # 214
    
    # scale up to 1000 * 400
    # meantree1$n_landscape <- meantree1$n * meantree1$EXP_FACT
    
    # Scale to 800 x 300
    meantree1$EXP_FACT[meantree1$EXP_FACT == 4000] <- 2400
    meantree1$EXP_FACT[meantree1$EXP_FACT == 2000] <- 1200
    meantree1$n_landscape <- meantree1$n * meantree1$EXP_FACT
    
    round(mean(meantree1$n_landscape)) # 363360
    
  # generating point cloud  
  x <- round(runif(round(mean(meantree1$n_landscape)), 0, 800))
  y <- round(runif(round(mean(meantree1$n_landscape)), 0, 300))

  # system.time(plot(x,y))

  system.time(loc <- cbind(x,y))
  system.time(loc <- as.data.frame(loc))

## Marks ####################

  ### Species ###############################
    # average number per species within plot
    meantreespp1 <- dbh %>% 
      filter(TREAT == 1) %>% # only 1x burns right now
      group_by(PLOT, SPP) %>% # group by plot / spp
      summarise(n = n()) %>% # counts number of spp within plot
        group_by(SPP) %>% # groups by spp only
        summarise(AV = round(mean(n))) # average of count
    meantreespp1$prob <- meantreespp1$AV / sum(meantreespp1$AV)# can't use 119 # doesn't add to 1
    sum(meantreespp1$prob) # checking if it adds to 1
    
   loc$SPP <- simple_ra(N = length(loc$y), 
              prob_each = meantreespp1$prob,
              conditions = meantreespp1$SPP) # names
   
   rm(meantreespp1)
 
 ### Live / Dead #############################
   dbh$LIVE_DEAD <- NA
   dbh$LIVE_DEAD[dbh$CANOPY > 0] <- "LIVE"
   dbh$LIVE_DEAD[dbh$CANOPY == 0] <- "DEAD"
   
   meanCan1 <- dbh %>% 
     filter(TREAT == 1) %>% # only 1x burns right now
     filter(CANOPY != "NA") %>%
     group_by(PLOT, SPP, LIVE_DEAD) %>% # group by plot / spp
     summarise(n = n()) %>% # counts number of spp within plot
     complete(expand(dbh, LIVE_DEAD), fill = list(n = 0 )) %>%
     filter(LIVE_DEAD != "NA") %>%
     pivot_wider(names_from = LIVE_DEAD, values_from = n)

   meanCan1$tot <- meanCan1$LIVE + meanCan1$DEAD
  
   meanCan1$prob <- NA ; meanCan1$prob <- round(meanCan1$LIVE / meanCan1$tot, digits = 2)
   
   meanCan1 <- meanCan1 %>%
     group_by(SPP) %>%
     summarise(mean(prob))
   
  pime_loc <- loc %>% 
    filter(SPP == "PIME") 
  pime_loc$LIVE_DEAD <- simple_ra(N = nrow(pime_loc),
                             prob = meanCan1$prob[meanCan1$SPP == "PIME"],
                            conditions = c(1,0))
   bene_loc <- loc %>% 
    filter(SPP == "BENE") 
  bene_loc$LIVE_DEAD <- simple_ra(N = nrow(bene_loc),
                                  prob_each = meanCan1$prob[meanCan1$SPP == "BENE"],
                                  conditions = c(1,0))
  potr_loc <- loc %>% 
    filter(SPP == "POTR") 
  potr_loc$LIVE_DEAD <- simple_ra(N = nrow(potr_loc),
                                  prob_each = meanCan1$prob[meanCan1$SPP == "POTR"],
                                  conditions = c(1,0))
  alcr_loc <- loc %>% 
    filter(SPP == "ALCR") 
  alcr_loc$LIVE_DEAD <- simple_ra(N = nrow(alcr_loc),
                                  prob_each = meanCan1$prob[meanCan1$SPP == "ALCR"],
                                  conditions = c(1,0))
  salix_loc <- loc %>% 
    filter(SPP == "SALIX") 
  salix_loc$LIVE_DEAD <- simple_ra(N = nrow(salix_loc),
                                  prob_each = meanCan1$prob[meanCan1$SPP == "SALIX"],
                                  conditions = c(1,0))
  # arcto_loc <- loc %>% 
    # filter(SPP == "ARCTO") 
  # arcto_loc$LIVE_DEAD <- simple_ra(N = nrow(arcto_loc),
                                  # prob_each = c(meanCan1$prob[meanCan1$SPP == "ARCTO"],
                                  #               1 - meanCan1$prob[meanCan1$SPP == "ARCTO"]),
                                  # conditions = c(1,0))
  loc <- rbind(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc)
  
    rm(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc,
       meanCan1)
  
  ### Height ##################################   
   # what's the average height per species within 1x burns
    height <- dbh %>% 
      filter(TREAT == 1) %>%
      filter(HEIGHT_M != "NA") %>% # one NA in PIME # need to check
      group_by(SPP, LIVE_DEAD) %>%
      summarise(AV = mean(HEIGHT_M), SD = sd(HEIGHT_M)) %>%
      complete(expand(dbh,LIVE_DEAD)) %>%
      filter(LIVE_DEAD != "NA")
    
    # No dead alcr or aspen in these plots, so they don't have heights
    # assigning the same mean and sd as the live trees
    height$AV[1] <- height$AV[2]
    height$SD[1] <- height$SD[2]
    
    height$AV[7] <- height$AV[8]
    height$SD[7] <- height$SD[8]
  
      # why is pime height NA
      # dbh1x <- dbh %>%
      #   filter(TREAT == 1)
      # dbh1x[which(is.na(dbh1x$HEIGHT_M)),]
      # 
      # dbh1x[dbh1x$SPP == "PIME",]
      
  #### Assigning heights ################
      # initializing column
        loc$HEIGHT_M <- 0
    
  ##### Birch 
  bene_loc <- loc %>%
    filter(SPP == "BENE")
  bene_height <- height %>%
    filter(SPP == "BENE")
  bene_loc$HEIGHT_M[bene_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                              a = 0, mean = bene_height$AV[bene_height$LIVE_DEAD=="DEAD"],
                                                              sd = bene_height$SD[bene_height$LIVE_DEAD=="DEAD"])
  bene_loc$HEIGHT_M[bene_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                              a = 0, mean = bene_height$AV[bene_height$LIVE_DEAD=="LIVE"],
                                                              sd = bene_height$SD[bene_height$LIVE_DEAD=="LIVE"])
  ##### Aspen
  potr_loc <- loc %>%
    filter(SPP == "POTR")
  potr_height <- height %>%
    filter(SPP == "POTR")
  potr_loc$HEIGHT_M[potr_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = potr_height$AV[potr_height$LIVE_DEAD=="DEAD"],
                                                          sd = potr_height$SD[potr_height$LIVE_DEAD=="DEAD"])
  potr_loc$HEIGHT_M[potr_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = potr_height$AV[potr_height$LIVE_DEAD=="LIVE"],
                                                          sd = potr_height$SD[potr_height$LIVE_DEAD=="LIVE"])
  ##### Spruce
  pime_loc <- loc %>%
    filter(SPP == "PIME")
  pime_height <- height %>%
    filter(SPP == "PIME")
  pime_loc$HEIGHT_M[pime_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = pime_height$AV[pime_height$LIVE_DEAD=="DEAD"],
                                                          sd = pime_height$SD[pime_height$LIVE_DEAD=="DEAD"])
  pime_loc$HEIGHT_M[pime_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = pime_height$AV[pime_height$LIVE_DEAD=="LIVE"],
                                                          sd = pime_height$SD[pime_height$LIVE_DEAD=="LIVE"])
  ##### Alder 
  alcr_loc <- loc %>%
    filter(SPP == "ALCR")
  alcr_height <- height %>%
    filter(SPP == "ALCR")
  alcr_loc$HEIGHT_M[alcr_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = alcr_height$AV[alcr_height$LIVE_DEAD=="DEAD"],
                                                          sd = alcr_height$SD[alcr_height$LIVE_DEAD=="DEAD"])
  alcr_loc$HEIGHT_M[alcr_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = alcr_height$AV[alcr_height$LIVE_DEAD=="LIVE"],
                                                          sd = alcr_height$SD[alcr_height$LIVE_DEAD=="LIVE"])
  
    # loc$HEIGHT_M[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree1$n)), a = 0,mean = height$AV[height$SPP == "ARCTO"],
                                #       sd = height$SD[height$SPP == "ARCTO"])
##### Willow 
  salix_loc <- loc %>%
    filter(SPP == "SALIX")
  salix_height <- height %>%
    filter(SPP == "SALIX")
  salix_loc$HEIGHT_M[salix_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = salix_height$AV[salix_height$LIVE_DEAD=="DEAD"],
                                                          sd = salix_height$SD[salix_height$LIVE_DEAD=="DEAD"])
  salix_loc$HEIGHT_M[salix_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = salix_height$AV[salix_height$LIVE_DEAD=="LIVE"],
                                                          sd = salix_height$SD[salix_height$LIVE_DEAD=="LIVE"])

  loc <- rbind(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc)
  
  rm(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc,
     height)
  rm(bene_height, potr_height, pime_height, salix_height, alcr_height)
  
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
    filter(TREAT == 1) %>%
    filter(!is.na(LIVE_DEAD)) %>%
    group_by(SPP, LIVE_DEAD) %>%
    summarise(AV = mean(DBH_CM), SD = sd(DBH_CM)) %>%
    complete(expand(dbh,LIVE_DEAD)) %>%
    filter(LIVE_DEAD != "NA")
  
  dbh_assign$AV[1] <- dbh_assign$AV[2]
  dbh_assign$SD[1] <- dbh_assign$SD[2]
  
  dbh_assign$AV[7] <- dbh_assign$AV[8]
  dbh_assign$SD[7] <- dbh_assign$SD[8]

  ##### Birch 
  bene_loc <- loc %>%
    filter(SPP == "BENE")
  bene_dbh <- dbh_assign %>%
    filter(SPP == "BENE")
  bene_loc$DBH_CM[bene_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = bene_dbh$AV[bene_dbh$LIVE_DEAD=="DEAD"],
                                                          sd = bene_dbh$SD[bene_dbh$LIVE_DEAD=="DEAD"])
  bene_loc$DBH_CM[bene_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = bene_dbh$AV[bene_dbh$LIVE_DEAD=="LIVE"],
                                                          sd = bene_dbh$SD[bene_dbh$LIVE_DEAD=="LIVE"])
  ##### Aspen 
  potr_loc <- loc %>%
    filter(SPP == "POTR")
  potr_dbh <- dbh_assign %>%
    filter(SPP == "POTR")
  potr_loc$DBH_CM[potr_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = potr_dbh$AV[potr_dbh$LIVE_DEAD=="DEAD"],
                                                          sd = potr_dbh$SD[potr_dbh$LIVE_DEAD=="DEAD"])
  potr_loc$DBH_CM[potr_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = potr_dbh$AV[potr_dbh$LIVE_DEAD=="LIVE"],
                                                          sd = potr_dbh$SD[potr_dbh$LIVE_DEAD=="LIVE"])
  
  ##### Spruce 
  pime_loc <- loc %>%
    filter(SPP == "PIME")
  pime_dbh <- dbh_assign %>%
    filter(SPP == "PIME")
  pime_loc$DBH_CM[pime_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = pime_dbh$AV[pime_dbh$LIVE_DEAD=="DEAD"],
                                                          sd = pime_dbh$SD[pime_dbh$LIVE_DEAD=="DEAD"])
  pime_loc$DBH_CM[pime_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = pime_dbh$AV[pime_dbh$LIVE_DEAD=="LIVE"],
                                                          sd = pime_dbh$SD[pime_dbh$LIVE_DEAD=="LIVE"])
  ##### Alder 
  alcr_loc <- loc %>%
    filter(SPP == "ALCR")
  alcr_dbh <- dbh_assign %>%
    filter(SPP == "ALCR")
  alcr_loc$DBH_CM[alcr_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = alcr_dbh$AV[alcr_dbh$LIVE_DEAD=="DEAD"],
                                                          sd = alcr_dbh$SD[alcr_dbh$LIVE_DEAD=="DEAD"])
  alcr_loc$DBH_CM[alcr_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                          a = 0, mean = alcr_dbh$AV[alcr_dbh$LIVE_DEAD=="LIVE"],
                                                          sd = alcr_dbh$SD[alcr_dbh$LIVE_DEAD=="LIVE"])
  
  # loc$DBH_CM[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree1$n)), a = 0,mean = dbh_assign$AV[dbh_assign$SPP == "ARCTO"],
  #       sd = dbh_assign$SD[dbh_assign$SPP == "ARCTO"])
  
  ##### Willow 
  salix_loc <- loc %>%
    filter(SPP == "SALIX")
  salix_dbh <- dbh_assign %>%
    filter(SPP == "SALIX")
  salix_loc$DBH_CM[salix_loc$LIVE_DEAD== 0] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                            a = 0, mean = salix_dbh$AV[salix_dbh$LIVE_DEAD=="DEAD"],
                                                            sd = salix_dbh$SD[salix_dbh$LIVE_DEAD=="DEAD"])
  salix_loc$DBH_CM[salix_loc$LIVE_DEAD== 1] <- rtruncnorm(round(mean(meantree1$n_landscape)), 
                                                            a = 0, mean = salix_dbh$AV[salix_dbh$LIVE_DEAD=="LIVE"],
                                                            sd = salix_dbh$SD[salix_dbh$LIVE_DEAD=="LIVE"])
  
  loc <- rbind(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc)
  
  rm(pime_dbh, potr_dbh, alcr_dbh, salix_dbh, bene_dbh)
  rm(pime_loc, potr_loc, alcr_loc, salix_loc, bene_loc,
     dbh_assign)
  
  #### Stem ###############################
  loc$STEM_BIOMASS <- 0
  loc$STEM_BIOMASS[loc$SPP == "PIME"] <-  117.91*(loc$DBH[loc$SPP == "PIME"]^1.99) # Alexander et al. 2012
  loc$STEM_BIOMASS[loc$SPP == "POTR"] <-  64.01*(loc$DBH[loc$SPP == "POTR"]^2.51) # Alexander et al. 2012
  loc$STEM_BIOMASS[loc$SPP == "BENE"] <-  147.96*(loc$DBH[loc$SPP == "BENE"]^2.25) # Alexander et al. 2012
  loc$STEM_BIOMASS[loc$SPP == "ALCR"] <-  exp(4.5 + 2.3*log(loc$DBH[loc$SPP == "ALCR"])) # Binkley et al. 1984
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
  loc$FOL_BIOMASS[loc$SPP == "POTR"] <-  18.98*(loc$DBH[loc$SPP == "POTR"]^1.53) # Alexander et al. 2012
  loc$FOL_BIOMASS[loc$SPP == "BENE"] <-  6.39*(loc$DBH[loc$SPP == "BENE"]^2.1) # Alexander et al. 2012
  loc$FOL_BIOMASS[loc$SPP == "ALCR"] <-  10^(1.82 + 2.38*log10(loc$DBH[loc$SPP == "ALCR"])) # Binkley et al. 1984
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
   
   write.csv(tree_input, here("data/output/tree_input100.csv"), row.names = F)
   
   rm(tree_input)
   
  #Assume that our 'area of interest' for collecting fire behavior data is in a subset of the 1000 m x 400 m domain
  # AOI: x{400,600}, y{100,300}
  # Each tree will also need a unique identifier
  # NOT tracking dead trees
  trees <- mutate(loc,
                 export = if_else((x >= 525 & x <= 600 & y >= 125 & y<=200 & LIVE_DEAD == 1),
                                  TRUE, FALSE)) %>%
    group_by(export) %>%
    mutate(id =  row_number())
  
  length(which(trees$export == "TRUE")) # 4477 # can do 4096 total
  
  ggplot(trees, aes(x,y,col=export)) + geom_point() + coord_equal()
  
  #Make tree list ready for WFDS
  
  # dbh from cm to m
  loc$DBH_M <- NA
  loc$DBH_M <- loc$DBH_CM / 100
    
  trees_WFDS = mutate(trees,
                 wfds_canopy =
                   paste0("&TREE XYZ=", round(x,1), ",", round(y,1),
                          ",0,PART_ID='TREE',FUEL_GEOM='CYLINDER',CROWN_WIDTH=", round(CROWN_WIDTH,1),
                          ",CROWN_BASE_HEIGHT=", round(CROWN_BASE_HEIGHT,1),
                          ",TREE_HEIGHT=", round(HEIGHT_M,1),
                          ",OUTPUT_TREE=.", export,
                          ".,LABEL='", paste0('tree',id), "' /"),
                 wfds_stem =
                   paste0("&TREE XYZ=",round(x,1), ",", round(y,1),
                          ",0,PART_ID='TRUNK',FUEL_GEOM='CYLINDER',CROWN_WIDTH=",round(DBH_M/100,1),
                          ",CROWN_BASE_HEIGHT=0",
                          ",TREE_HEIGHT=",CROWN_BASE_HEIGHT,"/"
                   )
  )
  
  write.csv(trees_WFDS, here("data/output/tree_WFDS.csv"), row.names = FALSE)
  
  #Note that all trees get labelled but only those trees in the area of interest (where export == TRUE)
  # will actually have their heating and mass tracked
  #Also, length units are in meters so dbh, commonly measured in cm, are expressed in m.

# 2x BURN #########################################
  ## Points #########################
  # mean number of trees per plot
  meantree1 <- dbh %>%
    filter(TREAT == 2) %>%
    group_by(PLOT) %>%
    summarise(n = n(), QUAD = unique(QUAD), EXP_FACT = unique(EXP_FACT))
  round(mean(meantree1$n)) # 194
  
  # scale up to 1000 * 400
  meantree1$n_landscape <- meantree1$n * meantree1$EXP_FACT
  
  round(mean(meantree1$n_landscape)) # 663,200
  
  # generating point cloud  
  x <- round(runif(round(mean(meantree1$n_landscape)), 0, 1000))
  y <- round(runif(round(mean(meantree1$n_landscape)), 0, 400))
  
  system.time(plot(x,y))
  
  system.time(loc <- cbind(x,y))
  system.time(loc <- as.data.frame(loc))
  
  ## Marks ####################
  
  ### Species ###############################
  # average number per species within plot
  meantreespp1 <- dbh %>% 
    filter(TREAT == 1) %>% # only 1x burns right now
    group_by(PLOT, SPP) %>% # group by plot / spp
    summarise(n = n()) %>% # counts number of spp within plot
    group_by(SPP) %>% # groups by spp only
    summarise(AV = round(mean(n))) # average of count
  meantreespp1$prob <- meantreespp1$AV / sum(meantreespp1$AV)# can't use 119 # doesn't add to 1
  sum(meantreespp1$prob) # checking if it adds to 1
  
  loc$SPP <- simple_ra(N = length(loc$y), 
                       prob_each = meantreespp1$prob,
                       conditions = meantreespp1$SPP) # names
  
  ### Live / Dead #############################
  meanCan1 <- dbh %>% 
    filter(TREAT == 1) %>% # only 1x burns right now
    filter(CANOPY > 0 ) %>%
    group_by(PLOT, SPP) %>% # group by plot / spp
    summarise(n = n()) %>% # counts number of spp within plot
    group_by(SPP) %>% # groups by spp only
    summarise(AV = round(mean(n))) 
  meanCan1$prob <- round(meanCan1$AV / sum(meanCan1$AV), digits = 2)
  
  pime_loc <- loc %>% 
    filter(SPP == "PIME") 
  pime_loc$LIVE_DEAD <- simple_ra(N = nrow(pime_loc),
                                  prob_each = c(meanCan1$prob[meanCan1$SPP == "PIME"],
                                                1 - meanCan1$prob[meanCan1$SPP == "PIME"]),
                                  conditions = c(1,0))
  bene_loc <- loc %>% 
    filter(SPP == "BENE") 
  bene_loc$LIVE_DEAD <- simple_ra(N = nrow(bene_loc),
                                  prob_each = c(meanCan1$prob[meanCan1$SPP == "BENE"],
                                                1 - meanCan1$prob[meanCan1$SPP == "BENE"]),
                                  conditions = c(1,0))
  potr_loc <- loc %>% 
    filter(SPP == "POTR") 
  potr_loc$LIVE_DEAD <- simple_ra(N = nrow(potr_loc),
                                  prob_each = c(meanCan1$prob[meanCan1$SPP == "POTR"],
                                                1 - meanCan1$prob[meanCan1$SPP == "POTR"]),
                                  conditions = c(1,0))
  alcr_loc <- loc %>% 
    filter(SPP == "ALCR") 
  alcr_loc$LIVE_DEAD <- simple_ra(N = nrow(alcr_loc),
                                  prob_each = c(meanCan1$prob[meanCan1$SPP == "ALCR"],
                                                1 - meanCan1$prob[meanCan1$SPP == "ALCR"]),
                                  conditions = c(1,0))
  salix_loc <- loc %>% 
    filter(SPP == "SALIX") 
  salix_loc$LIVE_DEAD <- simple_ra(N = nrow(salix_loc),
                                   prob_each = c(meanCan1$prob[meanCan1$SPP == "SALIX"],
                                                 1 - meanCan1$prob[meanCan1$SPP == "SALIX"]),
                                   conditions = c(1,0))
  # arcto_loc <- loc %>% 
  # filter(SPP == "ARCTO") 
  # arcto_loc$LIVE_DEAD <- simple_ra(N = nrow(arcto_loc),
  # prob_each = c(meanCan1$prob[meanCan1$SPP == "ARCTO"],
  #               1 - meanCan1$prob[meanCan1$SPP == "ARCTO"]),
  # conditions = c(1,0))
  loc <- rbind(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc)
  
  rm(pime_loc, bene_loc, potr_loc, salix_loc, alcr_loc,
     meanCan1)
  
  ### Height ##################################   
  # what's the average height per species within 1x burns
  height <- dbh %>% 
    filter(TREAT == 1) %>%
    filter(HEIGHT_M != "NA") %>% # one NA in PIME # need to check
    group_by(SPP) %>%
    summarise(AV = mean(HEIGHT_M), SD = sd(HEIGHT_M)) 
  
  # why is pime height NA
  # dbh1x <- dbh %>%
  #   filter(TREAT == 1)
  # dbh1x[which(is.na(dbh1x$HEIGHT_M)),]
  # 
  # dbh1x[dbh1x$SPP == "PIME",]
  
  #### Assigning heights ################
  # initializing column
  loc$HEIGHT_M <- 0
  
  loc$HEIGHT_M[loc$SPP=="BENE"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = height$AV[height$SPP == "BENE"],
                                              sd = height$SD[height$SPP == "BENE"])
  loc$HEIGHT_M[loc$SPP=="POTR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = height$AV[height$SPP == "POTR"],
                                              sd = height$SD[height$SPP == "POTR"])
  loc$HEIGHT_M[loc$SPP=="PIME"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = height$AV[height$SPP == "PIME"],
                                              sd = height$SD[height$SPP == "PIME"])
  loc$HEIGHT_M[loc$SPP=="ALCR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a =0, mean = height$AV[height$SPP == "ALCR"],
                                              sd = height$SD[height$SPP == "ALCR"])
  # loc$HEIGHT_M[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree1$n)), a = 0,mean = height$AV[height$SPP == "ARCTO"],
  #       sd = height$SD[height$SPP == "ARCTO"])
  loc$HEIGHT_M[loc$SPP=="SALIX"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = height$AV[height$SPP == "SALIX"],
                                               sd = height$SD[height$SPP == "SALIX"])
  loc$HEIGHT_M <- round(loc$HEIGHT_M, digits = 2)
  hist(loc$HEIGHT_M)
  
  rm(height)
  
  ### Biomass #########################
  #### Stem ###############################
  dbh$STEM_BIOMASS <- 0
  dbh$STEM_BIOMASS[dbh$SPP == "PIME"] <-  117.91*(dbh$DBH[dbh$SPP == "PIME"]^1.99) # Alexander et al. 2012
  dbh$STEM_BIOMASS[dbh$SPP == "POTR"] <-  64.01*(dbh$DBH[dbh$SPP == "POTR"]^2.51) # Alexander et al. 2012
  dbh$STEM_BIOMASS[dbh$SPP == "BENE"] <-  147.96*(dbh$DBH[dbh$SPP == "BENE"]^2.25) # Alexander et al. 2012
  dbh$STEM_BIOMASS[dbh$SPP == "ALCR"] <-  exp(4.5 + 2.3*log(dbh$DBH[dbh$SPP == "ALCR"])) # Binkley et al. 1984
  dbh$STEM_BIOMASS[dbh$SPP == "SALIX"] <- 10^(2.167 + 1.03*log10(dbh$DBH[dbh$SPP == "SALIX"])) # Bond-Lamberty et al. 2002
  
  biomass_stem <- dbh %>% 
    filter(TREAT == 1) %>%
    group_by(SPP) %>%
    summarise(AV = mean(STEM_BIOMASS), SD = sd(STEM_BIOMASS)) 
  
  loc$STEM_BIOMASS[loc$SPP=="BENE"] <- rtruncnorm(round(mean(meantree1$n_landscape)),a = 0, mean = biomass_stem$AV[height$SPP == "BENE"],
                                                  sd = biomass_stem$SD[height$SPP == "BENE"])
  loc$STEM_BIOMASS[loc$SPP=="POTR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "POTR"],
                                                  sd = biomass_stem$SD[biomass_stem$SPP == "POTR"])
  loc$STEM_BIOMASS[loc$SPP=="PIME"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "PIME"],
                                                  sd = biomass_stem$SD[biomass_stem$SPP == "PIME"])
  loc$STEM_BIOMASS[loc$SPP=="ALCR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a =0, mean = biomass_stem$AV[biomass_stem$SPP == "ALCR"],
                                                  sd = biomass_stem$SD[biomass_stem$SPP == "ALCR"])
  #loc$STEM_BIOMASS[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree1$n)), a = 0,mean = biomass_stem$AV[biomass_stem$SPP == "ARCTO"],
  # sd = biomass_stem$SD[biomass_stem$SPP == "ARCTO"])
  loc$STEM_BIOMASS[loc$SPP=="SALIX"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_stem$AV[biomass_stem$SPP == "SALIX"],
                                                   sd = biomass_stem$SD[biomass_stem$SPP == "SALIX"])
  #### Foliage ####################
  dbh$FOL_BIOMASS <- 0
  dbh$FOL_BIOMASS[dbh$SPP == "PIME"] <-  55.4*(dbh$DBH[dbh$SPP == "PIME"]^1.47) # Alexander et al. 2012
  dbh$FOL_BIOMASS[dbh$SPP == "POTR"] <-  18.98*(dbh$DBH[dbh$SPP == "POTR"]^1.53) # Alexander et al. 2012
  dbh$FOL_BIOMASS[dbh$SPP == "BENE"] <-  6.39*(dbh$DBH[dbh$SPP == "BENE"]^2.1) # Alexander et al. 2012
  dbh$FOL_BIOMASS[dbh$SPP == "ALCR"] <-  10^(1.82 + 2.38*log10(dbh$DBH[dbh$SPP == "ALCR"])) # Binkley et al. 1984
  dbh$FOL_BIOMASS[dbh$SPP == "SALIX"] <- 10^(2.023 + 1.065*log10(dbh$DBH[dbh$SPP == "SALIX"])) # Bond-Lamberty et al. 2002
  
  biomass_fol <- dbh %>% 
    filter(TREAT == 1) %>%
    group_by(SPP) %>%
    summarise(AV = mean(FOL_BIOMASS), SD = sd(FOL_BIOMASS)) 
  
  loc$FOL_BIOMASS[loc$SPP=="BENE"] <- rtruncnorm(round(mean(meantree1$n_landscape)),a = 0, mean = biomass_fol$AV[height$SPP == "BENE"],
                                                 sd = biomass_fol$SD[height$SPP == "BENE"])
  loc$FOL_BIOMASS[loc$SPP=="POTR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "POTR"],
                                                 sd = biomass_fol$SD[biomass_fol$SPP == "POTR"])
  loc$FOL_BIOMASS[loc$SPP=="PIME"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "PIME"],
                                                 sd = biomass_fol$SD[biomass_fol$SPP == "PIME"])
  loc$FOL_BIOMASS[loc$SPP=="ALCR"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a =0, mean = biomass_fol$AV[biomass_fol$SPP == "ALCR"],
                                                 sd = biomass_fol$SD[biomass_fol$SPP == "ALCR"])
  #loc$FOL_BIOMASS[loc$SPP=="ARCTO"] <- rtruncnorm(round(mean(meantree1$n)), a = 0,mean = biomass_fol$AV[biomass_fol$SPP == "ARCTO"],
  # sd = biomass_fol$SD[biomass_fol$SPP == "ARCTO"])
  loc$FOL_BIOMASS[loc$SPP=="SALIX"] <- rtruncnorm(round(mean(meantree1$n_landscape)), a = 0, mean = biomass_fol$AV[biomass_fol$SPP == "SALIX"],
                                                  sd = biomass_fol$SD[biomass_fol$SPP == "SALIX"])
  
  rm(biomass_fol, biomass_stem)  
  # setting dead trees to have foliar biomass = 0
  loc$FOL_BIOMASS[loc$LIVE_DEAD ==0] <- 0
  
  
  ### Crown Width #####################################
  
  loc$CROWN_WIDTH <- 2*(sqrt(loc$STEM_BIOMASS + loc$FOL_BIOMASS / pi * loc$HEIGHT))
  
  ## Exporting ##############################################
  
  #Assume that our 'area of interest' for collecting fire behavior data is in a subset of the 1000 m x 400 m domain
  # AOI: x{600,800}, y{100,300}
  # Each tree will also need a unique identifier
  trees <- mutate(loc,
                  export = if_else((x >= 600 & x <= 800 & y >= 100 & y<=300), TRUE, FALSE)) %>%
    group_by(export) %>%
    mutate(id =  row_number())
  
  length(which(trees$export == "TRUE")) # can do 4096 total
  
  ggplot(trees, aes(x,y,col=export)) + geom_point() + coord_equal()
  
  #Make tree list ready for WFDS
  trees = mutate(trees,
                 wfds_canopy =
                   paste0("&TREE XYZ=", round(x,1), ",", round(y,1),
                          ",0,PART_ID='TREE',FUEL_GEOM='CYLINDER',CROWN_WIDTH=", round(CROWN_WIDTH,1),
                          ",CROWN_BASE_HEIGHT=", round(cbh,1),
                          ",TREE_HEIGHT=", round(ht,1),
                          ",OUTPUT_TREE=.", export,
                          ".,LABEL='", paste0('tree',id), "' /"),
                 wfds_stem =
                   paste0("&TREE XYZ=",round(x,1), ",", round(y,1),
                          ",0,PART_ID='TRUNK',FUEL_GEOM='CYLINDER',CROWN_WIDTH=",round(dbh/100,1),
                          ",CROWN_BASE_HEIGHT=0",
                          ",TREE_HEIGHT=",cbh,"/"
                   )
  )
  
  #Note that all trees get labelled but only those trees in the area of interest (where export == TRUE)
  # will actually have their heating and mass tracked
  #Also, length units are in meters so dbh, commonly measured in cm, are expressed in m.
  
  
# 3x BURN ####################################################
  ## Points #########################
  # mean number of trees per plot
  meantree1 <- dbh %>%
    filter(TREAT == 3) %>%
    group_by(PLOT) %>%
    summarise(n = n(), QUAD = unique(QUAD), EXP_FACT = unique(EXP_FACT))
  round(mean(meantree1$n)) # 145
  
  # scale up to 1000 * 400
  meantree1$n_landscape <- meantree1$n * meantree1$EXP_FACT
  
  round(mean(meantree1$n_landscape)) # 511750
  