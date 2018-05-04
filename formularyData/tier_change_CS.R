setwd("~/Desktop/2018_spring/advancedData/finalProject/formularyData")

#load the required packages
library(dplyr)
library(readr)
library(lubridate)
library(reshape)

#load the data
humana <- read_csv("humana_clean.csv")
silverscript <- read_csv("silverscript_clean.csv")

#calculate tier change per drug, over time:

#reorder columns and extract year from date column

humana2 <- humana[,c(7,1,2)] %>%
  group_by(name) %>%
  mutate(year = year(year))

#reshape data using the melt and cast functions from the reshape package

humana_reshape1 <- melt(humana2,id.vars=c("name","year"))

humana_reshape2 <- cast(humana_reshape1,name~variable+year)

#calculate tier change from 2013 to 2014 (NOTE: There's probably a better way to do this. If I do it this way, I end up with a zero when there is no tier change, and a negative number when a drug either moves down a tier OR is dropped off the formulary completely. Since moving down a tier would mean cheaper prices, but falling off the formulary means removing access to a drug, I need to figure out a way to distinguish between the two in the dataset. Perhaps we can discuss on Sunday.)

humana_tier_change <- humana_reshape2 %>%
  mutate(tier_diff_2013_2018 = tier_num_2018 - tier_num_2013 ) %>%
  arrange(tier_diff_2013_2018)

#write csv

write.csv(humana_tier_change, "humana_tier_change.csv", na = "")

save.image("~/Desktop/2018_spring/advancedData/classData/week12/week12.RData")



