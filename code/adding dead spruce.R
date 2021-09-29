# adding dead spruce to 2019 file

library(here)
library(allometree)
library(tidyverse)

# 2019 data ########################################

herb2019 <- read.csv(here("data/dbh_clumpsas1.csv"))

which(is.na(herb2019$CANOPY)) # one canopy with missing entry
herb2019[3200,]

    # checking which plots have dead pime entered
    pime2019 <- herb2019 %>%
      filter(SPP== "PIME") %>%
      filter(HEIGHT_M >0) %>%
      filter(! is.na(CANOPY)) # one un-entered canopy from a spruce # 33_1

unique(pime2019$PLOT) # checking plots

# adding quadrant / exp_fact into herb2019 data
herb2019$QUAD <- 1
herb2019$EXP_FACT <- 100

herb2019$QUAD[herb2019$PLOT == "41_1"] <- 2
herb2019$QUAD[herb2019$PLOT == "20_1"] <- 2

herb2019$EXP_FACT[herb2019$PLOT == "41_1"] <- 100
herb2019$EXP_FACT[herb2019$PLOT == "20_1"] <- 100

# need to duplicate counts in certain plots to scale up to 2 quadrants

    plot42_1 <- herb2019[herb2019$PLOT == "42_1",]
    plot29_1 <- herb2019[herb2019$PLOT == "29_1",]
    plot36_1 <- herb2019[herb2019$PLOT == "36_1",]
    plot5_1 <- herb2019[herb2019$PLOT == "5_1",]
    plot52_1 <- herb2019[herb2019$PLOT == "52_1",]
    plot64_1 <- herb2019[herb2019$PLOT == "64_1",]
    plot65_1 <- herb2019[herb2019$PLOT == "65_1",]
    
    nrow(herb2019[herb2019$PLOT == "42_1",]) # test
    
    herb2019 <- rbind(herb2019, plot42_1, plot29_1, plot36_1, plot5_1, plot52_1,
                      plot64_1, plot65_1)
    
    nrow(herb2019[herb2019$PLOT == "42_1",]) # test
    
    herb2019$QUAD[herb2019$PLOT == "42_1"] <- 2
    herb2019$QUAD[herb2019$PLOT == "29_1"] <- 2
    herb2019$QUAD[herb2019$PLOT == "36_1"] <- 2
    herb2019$QUAD[herb2019$PLOT == "5_1"] <- 2
    herb2019$QUAD[herb2019$PLOT == "52_1"] <- 2
    herb2019$QUAD[herb2019$PLOT == "64_1"] <- 2
    herb2019$QUAD[herb2019$PLOT == "65_1"] <- 2
    
    rm(plot42_1, plot29_1, plot36_1, plot5_1, plot52_1,
       plot64_1, plot65_1)


# 2018 data ######################################

dbh2018 <- read.csv(here("data/dbh2018.csv"))

pime2018dead <- dbh2018 %>%
  filter(SPP == "PIME") %>%
  filter(CANOPY == 0)

unique(pime2018dead$PLOT)

add2018 <- pime2018dead %>%
  filter(PLOT != "32_2") %>%
  filter(PLOT != "10_0") %>%
  filter(PLOT != "11_0") %>%
  filter(PLOT != "41_1") %>%
  filter(PLOT != "33_1") %>%
  filter(PLOT != "4_2") %>%
  filter(PLOT != "1_0") %>%
  filter(PLOT != "26_2") %>%
  rename("DBH_CM" = "DBH", "STEM_TYPE" = "CLUMP") %>%
  select(!c(UND, BROWSE, START_END))

add2018$CLUMP_ID <- NA

# scaling 50_1 up to 1 quadrant
  plot50_1 <- add2018[add2018$PLOT == "50_1",]
  
  add2018 <- rbind(add2018, plot50_1[rep(1,4),])
  
  add2018$QUAD[add2018$PLOT == "50_1"] <- 1
  
  rm(plot50_1)

## height dbh equation ##############################

test <- ss_modelselect_multi(pime2019,
                             species = "SPP",
                             response = "HEIGHT_M",
                             predictor = "DBH_CM")
test

data(eqns_info, package = "allometree") # equation forms
eqns_info

testpred <- ss_simulate(ref_table = test$ss_models_info,
                         models = test$ss_models,
                         extrapolate = c(0,0.5))

test$ss_models_info

add2018$HEIGHT_M <- exp(1.308392 + 1.451135*log(log(add2018$DBH_CM + 1)))

ggplot() +
  geom_point(data = pime2019, aes(x = DBH_CM, y = HEIGHT_M)) +
  geom_ribbon(data = testpred, aes(x = predictor, ymin = lwr, ymax = upr), 
              alpha = 0.1) + 
  geom_line(data = testpred, aes(x = predictor, y = fit, lty = extrapolated)) +
  coord_cartesian(ylim = c(0, max(pime2019$HEIGHT_M)),
                  xlim = c(0, max(testpred$predictor))) + 
  geom_point(data = add2018, aes(x = DBH_CM, y = HEIGHT_M), col = "red") # checking added data


# checking files
colnames(add2018)
colnames(herb2019)

# Combining ############################################3
test <- rbind(herb2019, add2018)

test$EXP_FACT[test$QUAD == "1"] <- 4000
test$EXP_FACT[test$QUAD == "2"] <- 2000

write.csv(test, here("data/dbh_clumpsas1.csv"), row.names = FALSE)

