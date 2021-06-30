# working with species distances
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

# check your working directory
getwd() # tells you where you are

# pulls data in from csv file, assigns it as an object
dalt <- read.csv("data/dispersal/Dalton_spp_distance_2021.csv")
stee <- read.csv("data/dispersal/Steese_spp_dist_2021.csv")

# tells me how r is viewing the meter data
class(dalt$Distance_m)

# summarizes the data for distance in feet (mean, min, max, etc)
summary(dalt$Distance_ft)

# tells me unique entries for species column
unique(dalt$Species) # good way of checking if i've entered columns correctly
unique(stee$Species)

# checking the column names for both data frames
colnames(dalt)
colnames(steese)

# combining data files!!
combined <- rbind(dalt,stee)


ggplot(combined, aes(x = as.factor(Fires), y = Distance_m, fill = Species)) + 
  geom_boxplot() + labs(x = "Number of Fires", 
                        y = "Distance (meters)",
                        title = "Distance between Species")
write.csv(combined, "data/dispersal/Spp_dist_2021.csv", row.names = F)
