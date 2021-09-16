# Point process models

# Set up ##########################
library(tidyverse)
library(truncnorm)
library(randomizr) # https://cran.r-project.org/web/packages/randomizr/vignettes/randomizr_vignette.html

# Points #########################
  # mean number of trees per plot
  meantree1 <- dbh %>% 
    filter(TREAT == 1) %>%
    group_by(PLOT) %>%
    summarise(n = n())
  round(mean(meantree1$n)) # 119.2 # so 119

  # generating point cloud  
  x <- round(runif(119, 0, 20))
  y <- round(runif(119, 0, 20))

  plot(x,y)

  loc <- cbind(x,y)
  loc <- as.data.frame(loc)

# Marks ####################

dbh <- read.csv(here("data/dbh_clumpsas1.csv"))

  # Species ###############################
    # average number per species within plot
    meantreespp1 <- dbh %>% 
      filter(TREAT == 1) %>% # only 1x burns right now
      group_by(PLOT, SPP) %>% # group by plot / spp
      summarise(n = n()) %>% # counts number of spp within plot
        group_by(SPP) %>% # groups by spp only
        summarise(AV = round(mean(n))) # average of count
    meantreespp1$prob <- round(meantreespp1$AV / 128,# can't use 119 # doesn't add to 1
                               digits = 2)
    sum(meantreespp1$prob) # checking if it adds to 1
    
   loc$SPP <- simple_ra(N = 119, 
              prob_each = meantreespp1$prob,
              conditions = meantreespp1$SPP) # names
 
 # Live / Dead #############################
   meanCan1 <- dbh %>% 
     filter(TREAT == 1) %>% # only 1x burns right now
     group_by(PLOT, SPP) %>% # group by plot / spp
     summarise(n = n()) %>% # counts number of spp within plot
     group_by(SPP) %>% # groups by spp only
     summarise(AV = round(mean(n))) # average of count

  
  # Height ##################################   
   # what's the average height per species within 1x burns
    height <- dbh %>% 
      filter(TREAT == 1) %>%
      filter(HEIGHT_M != "NA") %>% # one NA in PIME # need to check
      group_by(SPP) %>%
      summarise(AV = mean(HEIGHT_M), SD = sd(HEIGHT_M)) 
  
      # why is pime height NA
      dbh1x <- dbh %>%
        filter(TREAT == 1)
      dbh1x[which(is.na(dbh1x$HEIGHT_M)),]
      
      dbh1x[dbh1x$SPP == "PIME",]
  
  # Assigning heights ################3
      # initializing column
        loc$HEIGHT <- 0
      
  loc$HEIGHT[loc$SPP=="BENE"] <- rtruncnorm(119,a = 0, mean = height$AV[height$SPP == "BENE"],
                                                  sd = height$SD[height$SPP == "BENE"])
  loc$HEIGHT[loc$SPP=="POTR"] <- rtruncnorm(119, a = 0, mean = height$AV[height$SPP == "POTR"],
                                       sd = height$SD[height$SPP == "POTR"])
  loc$HEIGHT[loc$SPP=="PIME"] <- rtruncnorm(119, a = 0, mean = height$AV[height$SPP == "PIME"],
                                       sd = height$SD[height$SPP == "PIME"])
  loc$HEIGHT[loc$SPP=="ALCR"] <- rtruncnorm(119, a =0, mean = height$AV[height$SPP == "ALCR"],
                                       sd = height$SD[height$SPP == "ALCR"])
  loc$HEIGHT[loc$SPP=="ARCTO"] <- rtruncnorm(119, a = 0,mean = height$AV[height$SPP == "ARCTO"],
                                       sd = height$SD[height$SPP == "ARCTO"])
  loc$HEIGHT[loc$SPP=="SALIX"] <- rtruncnorm(119, a = 0, mean = height$AV[height$SPP == "SALIX"],
                                       sd = height$SD[height$SPP == "SALIX"])
  loc$HEIGHT <- round(loc$HEIGHT, digits = 2)
  hist(loc$HEIGHT)

  # Next, need to assign biomass

