setwd("~/Desktop/2018_spring/advancedData/finalProject/formularyData")

#load the required packages
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

#load the data
humana <- read_csv("humana_clean.csv") %>%
  mutate(year = year(year))

##################
# humana

humana_tiers_spread <- humana %>%
  group_by(name, year) %>%
  summarize(tier_num = median(tier_num)) %>%
  spread(year, tier_num)

humana_tiers_gathered <- humana_tiers_spread %>%
  gather(year, tier_num, -name) %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(change = as.integer(tier_num-lag(tier_num)),
         change = ifelse(change == 0, NA, change),
         added = ifelse(tier_num >= 1 & is.na(lag(tier_num)),1,0),
         added = ifelse(year == 2013, NA, added),
         dropped = ifelse(is.na(tier_num) & lag(tier_num) >= 1,1,0))

humana_drug_summary <- humana_tiers_gathered %>%
  group_by(name) %>%
  summarize(tier_range = max(tier_num, na.rm=TRUE) - min(tier_num, na.rm=TRUE),
            n_added = sum(added, na.rm= TRUE),
            n_dropped = sum(dropped, na.rm= TRUE),
            n_tier_changed = length(which(!is.na(change))))

#write csv

write.csv(humana_drug_summary, "humana_drug_summary.csv", na = "")

write.csv(humana_tiers_spread, "humana_tiers_spread.csv", na = "")


######################

#analyze the data

#find the total number of drugs that have been on the formulary for at least one year between 2013-2018

humana_total_drugs <- humana_drug_summary %>%
  summarize(count = n())

#find the total number of drugs that were added to the formulary once after 2013 (excludes drugs that were added, dropped and then added back in)
humana_total_added <- humana_drug_summary %>%
  filter(n_added == 1) %>%
  group_by(n_added) %>%
  summarize(count = n())

#find the total number of drugs that were dropped form the formulary between 2013-2018 (excluding drugs added after 2013)

humana_total_dropped <- humana_drug_summary %>%
  filter(n_added == 0, n_dropped == 1) %>%
  group_by(n_dropped) %>%
  summarize(count = n())

# find the number of drugs that were on the formulary in 2013, and are still on the formulary now, in the same tier.
humana_total_no_change <- humana_drug_summary %>%
  filter(n_dropped == 0 & n_added == 0 & n_tier_changed == 0) %>%
  summarize(count_no_change = n())

#check how my imaginary Medicare patient would fare if they stayed with Humana from 2013-2018
humana_patient <- humana_tiers_spread %>%
  filter(grepl("HUMALOG|HUMULIN|ATORVASTATIN|LOSARTAN", name, ignore.case=TRUE))

save.image("~/Desktop/2018_spring/advancedData/classData/week12/week12.RData")


