# clumping dbh with height (from herbivory inventory)

library(tidyverse)
library(here)

dbh <- read.csv(here("data/herb_unclump.csv"))

# double checking bumbers
test <- seq(1:1030)

start <- dbh %>% 
  filter(CLUMP_TYPE == "START")

length(start$CLUMP_ID[start$CLUMP_ID == test])
check <- start$CLUMP_ID[start$CLUMP_ID == test]
tail(check)

end <- dbh %>% 
  filter(CLUMP_TYPE == "END")

length(end$CLUMP_ID[end$CLUMP_ID == test])
end$CLUMP_ID[end$CLUMP_ID == test]

dbh$CLUMP_ID <- as.numeric(dbh$CLUMP_ID)

clump <- dbh %>%
  filter(STEM_TYPE == "CLUMP")

length(unique(clump$CLUMP_ID))
tail(unique(clump$CLUMP_ID))

n <- clump %>%
  group_by(CLUMP_ID) %>%
  summarise(n())
n <- as.data.frame(n)

n[n$`n()` == 1,]

dbh %>%
  filter(STEM_TYPE == "CLUMP") %>%
  group_by(SITE, TREAT, PLOT, CLUMP_ID) %>%
  summarise(DBH_CM = mean(DBH_CM), CANOPY = mean(CANOPY), 
            HEIGHT_M = mean(HEIGHT_M), SPP = SPP)



