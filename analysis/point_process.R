# Point process models

# Set up ##########################
library(tidyverse)
library(truncnorm)
library(here)
library(randomizr) # https://cran.r-project.org/web/packages/randomizr/vignettes/randomizr_vignette.html

dbh <- read.csv(here("data/dbh_clumpsas1.csv"))

# taking ARCTO out for now
dbh <- dbh %>%
  filter(SPP != "ARCTO")

# Points #########################
  # mean number of trees per plot
    meantree1 <- dbh %>%
      filter(TREAT == 1) %>%
      group_by(PLOT) %>%
      summarise(n = n(), QUAD = unique(QUAD), EXP_FACT = unique(EXP_FACT))
    round(mean(meantree1$n)) # 399
    
    # scale up to 1000 * 400
    meantree1$n_landscape <- meantree1$n * meantree1$EXP_FACT
    
    round(mean(meantree1$n_landscape)) # 1031467
    
  # generating point cloud  
  x <- round(runif(round(mean(meantree1$n_landscape)), 0, 1000))
  y <- round(runif(round(mean(meantree1$n_landscape)), 0, 400))

  system.time(plot(x,y))

  system.time(loc <- cbind(x,y))
  system.time(loc <- as.data.frame(loc))

# Marks ####################

  ## Species ###############################
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
 
 ## Live / Dead #############################
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
  
  ## Height ##################################   
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
      
  ### Assigning heights ################
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

## Biomass #########################
  ### Stem ###############################
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
  ### Foliage ####################
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

  # setting dead trees to have foliar biomass = 0
  loc$FOL_BIOMASS[loc$LIVE_DEAD ==0] <- 0
  
  
  ## Crown Width #####################################
  
  loc$CROWN_WIDTH <- 2*(sqrt(loc$STEM_BIOMASS + loc$FOL_BIOMASS / pi * loc$HEIGHT))
  
