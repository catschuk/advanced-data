> setwd("~/Desktop/2018_spring/advancedData/finalProject/formularyData")

# load required packages
library(rvest)
library(dplyr)

# code to scrape one Humana page (Preferred)

# grab the html code
tmpH <- read_html("https://q1medicare.com/PartD-BrowseMedicare-2018PlanFormulary.php?letter=A&formulary=00018254&contractId=S5884&planId=114&segmentId=0&zipCountyCode=0&ccountyName=Statewide&stateReg=32CA&zip=&planType=P&mode=state&prAuth=E&stepTh=E&qtyLmt=E&tier1=&tier2=&tier3=&tier4=&tier5=&tier6=&sort=drugNameasc") %>%
  # read the table
  html_table(fill = TRUE) %>%
  # convert it to a data frame
  as.data.frame() %>%
  # only the first 6 columns contain data, so select those
  select(1:6) %>%
  # remove the extra header rows
  slice(5:n()) 
# rename columns
names(tmpH) <- c("name","tier_num","tier_desc","cost_30","cost_90","management")
# filter out the header rows within the table
tmpH <- tmp %>%
  filter(name != "Drug Name")

##########################################
# loops to scrape formularies

# these variables common to both formularies

# base url
base <- "https://q1medicare.com/PartD-BrowseMedicare-"

# middle of url
middle <- "PlanFormulary.php?letter="

# url suffix
suffix <- "&segmentId=0&zipCountyCode=0&ccountyName=Statewide&stateReg=32CA&zip=&planType=P&mode=state&prAuth=E&stepTh=E&qtyLmt=E&tier1=&tier2=&tier3=&tier4=&tier5=&tier6=&sort=drugNameasc"

# letters
letters <- c("0-9",LETTERS)

# these specific to humana

humana1 <- data_frame(year=c(2018:2013),
                            form=c("&formulary=00018254",
                                   "&formulary=00017216",
                                   "&formulary=00016344",
                                   "&formulary=00015080",
                                   "&formulary=00014054",
                                   "&formulary=00013259"))

humana2 <- "contractId=S5884&planId=114"

# empty data frame to get results of scrape
humana_df <- data_frame()

for (y in humana1$year) {
  tmpH1 <- humana1 %>%
    filter(year == y)
  for (l in letters) {
    try(tmpH2 <- read_html(paste0(base,
                                 tmpH1$year,
                                 middle,
                                 l,
                                 tmpH1$form,
                                 humana2,
                                 suffix)) %>%
          html_table(fill = TRUE) %>%
          as.data.frame() %>%
          select(1:6) %>%
          slice(5:n())) 
    try(names(tmpH2) <- c("name","tier_num","tier_desc","cost_30","cost_90","management"))
    try(tmpH2 <- tmpH2 %>%
          filter(name != "Drug Name") %>%
          mutate(year = tmpH1$year))
    try(humana_df <- bind_rows(humana_df,tmpH2))
    print(paste0(y," ",l))
    rm(tmpH2)
  }
}

#write csvs

# load required packages
library(readr)

write_csv(humana_df, "humana.csv", na="")

save.image("~/Desktop/2018_spring/advancedData/final_project/formularyData/formulariesScraper.RData")
