

# load required packages
library(rvest)
library(dplyr)

########################################## 
# code to scrape one Silverscript page

# grab the html code
tmp <- read_html("https://q1medicare.com/PartD-BrowseMedicare-2018PlanFormulary.php?letter=A&formulary=00018419&contractId=S5601&planId=064&segmentId=0&zipCountyCode=0&ccountyName=Statewide&stateReg=32CA&zip=&planType=P&mode=state&prAuth=E&stepTh=E&qtyLmt=E&tier1=&tier2=&tier3=&tier4=&tier5=&tier6=&sort=drugNameasc") %>%
  # read the table
  html_table(fill = TRUE) %>%
  # convert it to a data frame
  as.data.frame() %>%
  # only the first 6 columns contain data, so select those
  select(1:6) %>%
  # remove the extra header rows
  slice(5:n()) 
# rename columns
names(tmp) <- c("name","tier_num","tier_desc","cost_30","cost_90","management")
# filter out the header rows within the table
tmp <- tmp %>%
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

# these specific to silverscript

silverscript1 <- data_frame(year=c(2018:2013),
                            form=c("&formulary=00018419",
                                   "&formulary=00017255",
                                   "&formulary=00016125",
                                   "&formulary=00015301",
                                   "&formulary=00014232",
                                   "&formulary=00013572"))

silverscript2 <- "contractId=S5601&planId=064"

# empty data frame to get results of scrape
silverscript_df <- data_frame()

for (y in silverscript1$year) {
  tmp1 <- silverscript1 %>%
    filter(year == y)
  for (l in letters) {
    try(tmp2 <- read_html(paste0(base,
                                 tmp1$year,
                                 middle,
                                 l,
                                 tmp1$form,
                                 silverscript2,
                                 suffix)) %>%
          html_table(fill = TRUE) %>%
          as.data.frame() %>%
          select(1:6) %>%
          slice(5:n())) 
    try(names(tmp2) <- c("name","tier_num","tier_desc","cost_30","cost_90","management"))
    try(tmp2 <- tmp2 %>%
          filter(name != "Drug Name") %>%
          mutate(year = tmp1$year))
    try(silverscript_df <- bind_rows(silverscript_df,tmp2))
    print(paste0(y," ",l))
    rm(tmp2)
  }
}


########################################## 
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

write_csv(silverscript_df, "silverscript.csv", na="")

save.image("~/Desktop/2018_spring/advancedData/final_project/formularyData/formulariesScraper.RData")
