# adding dead spruce to 2019 file

library(here)
library(tidyverse)

herb2019 <- read.csv(here("data/dbh_clumpsas1.csv"))

which(is.na(herb2019$CANOPY))
herb2019[3200,]

# checking which plots have dead pime entered
pime2019 <- herb2019 %>%
  filter(SPP== "PIME") %>%
  filter(HEIGHT_M >0) %>%
  filter(! is.na(CANOPY)) # one un-entered canopy from a spruce # 33_1

unique(pime2019$PLOT)

pime2019dead <- pime2019[pime2019$CANOPY == 0,]
plots2019 <- unique(pime2019dead$PLOT) # dead spruce in 8 plots

# adding quadrant / exp_fact into herb2019 data
herb2019$QUAD <- 2
herb2019$EXP_FACT <- 50

# bringing in 2018

dbh2018 <- read.csv(here("data/dbh2018.csv"))

pime2018dead <- dbh2018 %>%
  filter(SPP == "PIME") %>%
  filter(CANOPY == 0)

unique(pime2018dead$PLOT)

# adding height to 2018 spruce


test <- ss_modelselect_multi(pime2019,
                             species = "SPP",
                             response = "HEIGHT_M",
                             predictor = "DBH_CM")
test

testpred <- ss_simulate(ref_table = test$ss_models_info,
                         models = test$ss_models,
                         extrapolate = c(0,0.5))

add2018$HEIGHT_M <- exp(1.308392 + 1.451135*log(log(add2018$DBH_CM + 1)))

ggplot() +
  geom_point(data = pime2019, aes(x = DBH_CM, y = HEIGHT_M)) +
  geom_ribbon(data = testpred, aes(x = predictor, ymin = lwr, ymax = upr), 
              alpha = 0.1) + 
  geom_line(data = testpred, aes(x = predictor, y = fit, lty = extrapolated)) +
  coord_cartesian(ylim = c(0, max(pime2019$HEIGHT_M)),
                  xlim = c(0, max(testpred$predictor))) + 
  geom_point(data = add2018, aes(x = DBH_CM, y = HEIGHT_M), col = "red")

test$ss_models_info

data(eqns_info, package = "allometree")
eqns_info

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

add2018$HEIGHT_M <- NA
add2018$CLUMP_ID <- NA
colnames(add2018)
colnames(herb2019)

test <- rbind(herb2019, add2018)


