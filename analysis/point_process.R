# Point process models

library(tidyverse)
library(truncnorm)
library(randomizr)

# generating point cloud  
x <- round(runif(119, 0, 20))
y <- round(runif(119, 0, 20))

plot(x,y)

loc <- cbind(x,y)
loc <- as.data.frame(loc)

# informing marks

dbh <- read.csv(here("data/dbh_clumpsas1.csv"))

  # mean number of trees per plot
  meantree1 <- dbh %>% 
    filter(TREAT == 1) %>%
    group_by(PLOT) %>%
    summarise(n = n())
  round(mean(meantree1$n)) # 119.2
  
  # average number per species within plot
  meantreespp1 <- dbh %>% 
    filter(TREAT == 1) %>%
    group_by(PLOT, SPP) %>%
    summarise(n = n()) %>%
      group_by(SPP) %>%
      summarise(AV = round(mean(n)))
  meantreespp1$prop <- round(meantreespp1$AV / 128,
                             digits = 2)
  sum(meantreespp1$prop)
  
 loc$SPP <- simple_ra(N = 119, 
            prob_each = meantreespp1$prop,
            conditions = meantreespp1$SPP)

  # what's the average height per species within 1x burns
  height <- dbh %>% 
    filter(TREAT == 1) %>%
    filter(HEIGHT_M != "NA") %>%
    group_by(SPP) %>%
    summarise(AV = mean(HEIGHT_M), SD = sd(HEIGHT_M)) 
  
  # why is pime height NA
  dbh1x <- dbh %>%
    filter(TREAT == 1)
  dbh1x[which(is.na(dbh1x$HEIGHT_M)),]
  
  dbh1x[dbh1x$SPP == "PIME",]
  
  loc$HEIGHT <- "NA"
  
  loc$HEIGHT[loc$SPP=="POTR"] <- rtruncnorm(119, a = 0, mean = height$AV[height$SPP == "POTR"],
                                       sd = height$SD[height$SPP == "POTR"])
  loc$HEIGHT[loc$SPP=="PIME"] <- rtruncnorm(119, a = 0, mean = height$AV[height$SPP == "PIME"],
                                       sd = height$SD[height$SPP == "PIME"])
  loc$HEIGHT[loc$SPP=="ALCR"] <- rtruncnorm(119, a =0, mean = height$AV[height$SPP == "ALCR"],
                                       sd = height$SD[height$SPP == "ALCR"])
  loc$HEIGHT[loc$SPP=="ARCTO"] <- rtruncnorm(119, a = 0,mean = height$AV[height$SPP == "ARCTO"],
                                       sd = height$SD[height$SPP == "ARCTO"])
  loc$HEIGHT[loc$SPP=="BENE"] <- rtruncnorm(119,a = 0, mean = height$AV[height$SPP == "BENE"],
                                        sd = height$SD[height$SPP == "BENE"])
  loc$HEIGHT[loc$SPP=="SALIX"] <- rtruncnorm(119, a = 0, mean = height$AV[height$SPP == "SALIX"],
                                       sd = height$SD[height$SPP == "SALIX"])

  loc$HEIGHT <- as.numeric(loc$HEIGHT)
  loc$HEIGHT <- round(loc$HEIGHT, digits = 2)
  hist(loc$HEIGHT)


