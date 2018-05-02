![](https://s14.postimg.cc/prjfran3l/Screen_Shot_2018-05-01_at_6.46.45_PM.png)

#D is for denial
#####*In California, patients with Medicare Part D plans are losing access to the drugs they need*
######By Cat Schuknecht


- - -

Imagine you’re Medicare patient in your mid-sixties with Type 1 diabetes. You were diagnosed as an adult, but prescription drugs have been part of your daily routine for the past 20 years. Every meal requires you to interact with a complex regimen of expensive drugs and technologies that you and your doctor have designed to best fit your needs. On top of that, you take a few other drugs for high cholesterol and high blood pressure — pretty normal for someone of late middle-age.  It’s a lot to keep track of, but your Medicare prescription drug plan helps you cover a lot of the cost.

Then one day, you walk up to the counter of your local pharmacy — the same place you’ve been buying your medications since you were first diagnosed — and the pharmacist tells you that your Medicare Part D plan list of covered drugs — called a drug formulary — has changed. The cost of your medication is going up, and your insulin won’t be covered at all, so you’ll have to switch to a different brand.

This is the situation my mom found herself in recently. When she told me, I wondered how common her experience was. How often do Medicare patients find themselves losing access to the drugs they rely on?

According to Jack Hoadley, a health policy analyst at Georgetown University who specializes in Medicare prescription drug issues, it happens a lot. He says it can be a serious burden for people with chronic illnesses like diabetes: “It’s a lot different than popping a blue pill one day and then popping a pink pill the next,” said Hoadley.

Finding a plan that covers all of your medications can be really difficult. Casey Schwarz, an attorney at the Medicare Rights Center, says it can even be impossible.

“Plans can’t get rid of all diabetes medications,” said Schwarz. “But we see a lot of people who take a lot of different medications, and finding a plan that can cover all [their] meds is sometimes impossible.” Patients can always submit an appeal to their insurance company, but that can be tough for patients who are already managing a chronic illness.

The problem is, no one outside the Centers for Medicare and Medicaid Services (CMS) is tracking how Medicare Part D plan formularies change over time, so there’s a lack of accountability. (The center makes its data available, but for a price — $250 for one month’s worth of data.)[1]

This doesn’t mean Medicare plans can do whatever they want. There are some rules; they’re not allowed to stop covering drugs in the middle of the year without CMS approval, and, when they do make formulary changes, they’re required to give patients advanced notice.

But, the rules that govern Medicare drug plans could change soon. CMS recently proposed a new rule that would give Part D plans more flexibility in crafting their benefit structures, which would mean less flexibility for patients. If the rule goes into effect, more patients could start finding themselves in the same situation that my mom found herself in last year — empty-handed at the pharmacy.

--

[1]I’ve gone back and forth with a CMS press officer about getting the fee waived, but he ultimately said that they couldn’t do it because “a serious amount of work needs to be done to make the data non-identifiable and to ensure it does not contain any protected health information or personally identifiable information.” (The data is described [here](https://www.cms.gov/Research-Statistics-Data-and-Systems/Files-for-Order/NonIdentifiableDataFiles/PrescriptionDrugPlanFormularyPharmacyNetworkandPricingInformationFiles.html).)


-----------
###**The data**

For this story, I created an original dataset in order to answer a central question: how do patients who don’t change Medicare drug plans fare over time?

Current formulary information is available online through the [CMS Plan Finder](https://www.medicare.gov/find-a-plan/questions/home.aspx?AspxAutoDetectCookieSupport=1), and insurance companies post up-to-date formulary information on their websites. But, historic formulary data is much harder to find. (CMS charges $250 for just one month of data.) Since I wanted to track formulary changes over time, I had to get creative.

I found a website called [Q1Medicare](https://q1medicare.com/PartD-BrowseMedicare-2018PlanFormulary.php), which is operated by Q1Group, LLC and advertises itself as an educational tool.(I corresponded with several of their staff members, and they said that they get all of their data from Medicare.) The website allowed me to look up Medicare Part D formulary data by state and by plan, and I was able to manipulate the URL to get data for past years, as far back as 2013.

I decided to look at formulary data for one of California’s top enrolled Medicare plans — Humana. Using county-level [enrollment data](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MCRAdvPartDEnrolData/Monthly-PDP-Enrollment-by-State-County-Contract.html?DLSort=1&DLEntries=10&DLPage=1&DLSortDir=descending) from CMS for January, 2018, I used used a pivot table to figure out which companies have the highest number of enrollees in California, and then calculated the cumulative percent using the formula **=SUM($B$2:B2)/2268036*100 **in Excel:

![](https://s14.postimg.cc/b23701x7l/Screen_Shot_2018-05-01_at_8.19.25_PM.png)

I found that, together, SilverScript and Humana cover almost 50 percent of Medicare patients in California. Q1Medicare had more complete data for Humana (it was missing SilverScript data for 2014 and 2015), so I decided to start with Humana’s cheapest plan —Humana Preferred Rx Plan (S5884-114-0). I also picked Humana because, in 2015, CMS imposed a civil money penalty of  $3 million against Humana for violating Medicare Part D formulary requirements, among other things. The [letter](https://www.cms.gov/Medicare/Compliance-and-Audits/Part-C-and-Part-D-Compliance-and-Audits/Downloads/Humana_CMP_12_29_2015.pdf) from CMS to Humana’s CEO, Bruce Broussard, accused Humana of policies that “resulted in Humana’s enrollees experiencing inappropriate denials of coverage at the point of sale.”

Next, I needed to get the data from Q1Medicare's website into a usable format. This is how it appears on their website:

![](https://s14.postimg.cc/fhh0rqrzl/Screen_Shot_2018-05-01_at_6.35.19_PM.png)


So, I scraped data from the Q1Medicare website for Humana’s Preferred Rx Plan using the following code, written with help from Peter Aldhous of Buzzfeed News:

```
#load required packages
library(rvest)
library(dplyr)

#code to scrape one Humana page:

#grab the html code
tmpH <- read_html("https://q1medicare.com/PartD-BrowseMedicare-2018PlanFormulary.php?letter=A&formulary=00018254&contractId=S5884&planId=114&segmentId=0&zipCountyCode=0&ccountyName=Statewide&stateReg=32CA&zip=&planType=P&mode=state&prAuth=E&stepTh=E&qtyLmt=E&tier1=&tier2=&tier3=&tier4=&tier5=&tier6=&sort=drugNameasc") %>%

#read the table
html_table(fill = TRUE) %>%

#convert it to a data frame
as.data.frame() %>%

#only the first 6 columns on the webpage contain data, so select those
select(1:6) %>%

#remove the extra header rows
slice(5:n())

#rename columns
names(tmpH) <- c("name","tier_num","tier_desc","cost_30","cost_90","management")

#filter out the header rows within the table
tmpH <- tmp %>%
  filter(name != "Drug Name")

##########################################

#create loops to scrape formularies for Humana drugs on pages A-Z and 0-9:

#the following variables are common to all formularies in the Q1Medicare database:

#base url
base <- "https://q1medicare.com/PartD-BrowseMedicare-"

#middle of url
middle <- "PlanFormulary.php?letter="

#url suffix
suffix <- "&segmentId=0&zipCountyCode=0&ccountyName=Statewide&stateReg=32CA&zip=&planType=P&mode=state&prAuth=E&stepTh=E&qtyLmt=E&tier1=&tier2=&tier3=&tier4=&tier5=&tier6=&sort=drugNameasc"

#letters
letters <- c("0-9",LETTERS)

#these are specific to Humana:

humana1 <- data_frame(year=c(2018:2013),
                            form=c("&formulary=00018254",
                                   "&formulary=00017216",
                                   "&formulary=00016344",
                                   "&formulary=00015080",
                                   "&formulary=00014054",
                                   "&formulary=00013259"))

humana2 <- "contractId=S5884&planId=114"

#create an empty data frame fill with the results of the scrape
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

```

This was the result:


![](https://s14.postimg.cc/9gjbumncx/Screen_Shot_2018-05-01_at_6.31.49_PM.png)

-----------
###**The analysis**

Once I created my dataset, I needed to restructure it in order to analyze tier change over time. (This is how insurance companies categorize their drugs in terms of cost. The first tier usually includes generic medications, which are usually the cheapest. As the tiers move up from two to five, the drugs increase in price. So, if a drug moves up a tier, that means it's getting more expensive. If it moves down a tier, it's getting cheaper.) After cleaning it in Open Refine, I tried using the lubridate and reshape packages to 'melt' and 'cast' my data from narrow to wide:

```
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


```
This sort of worked, but if I left it this way, it would be tough to analyze later on. With help from Peter Aldhous, I tried using the tidyr package instead:
```
#load the required packages
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

#load the data
humana <- read_csv("humana_clean.csv") %>%
  mutate(year = year(year))

# reshape the data from narrow to wide
humana_tiers_spread <- humana %>%
  group_by(name, year) %>%
  summarize(tier_num = median(tier_num)) %>%
  spread(year, tier_num)

#gather the data back to narrow (this will allow us to check our analysis later)

humana_tiers_gathered <- humana_tiers_spread %>%
  gather(year, tier_num, -name) %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(change = as.integer(tier_num-lag(tier_num)),
         change = ifelse(change == 0, NA, change),
         added = ifelse(tier_num >= 1 & is.na(lag(tier_num)),1,0),
         added = ifelse(year == 2013, NA, added),
         dropped = ifelse(is.na(tier_num) & lag(tier_num) >= 1,1,0))

```

The result was much better using the tidyr package. This was the result for humana_tiers_spread. As you can see, the data was transformed from narrow to wide.
![](https://s14.postimg.cc/ynu7v9gap/Screen_Shot_2018-05-01_at_7.09.07_PM.png)


Next, I calculated tier change over time for each drug. For each drug, I found the number of times its tier level changed, the range between the highest and lowest tier, and the number of times it was added to or dropped from the formulary. Then, I did some summary calculations:

```

#find the number of times each drug has been added and dropped from Humana's formulary, as well as the range between the highest and lowest tier, and the number of times it has changed tier
humana_drug_summary <- humana_tiers_gathered %>%
  group_by(name) %>%
  summarize(tier_range = max(tier_num, na.rm=TRUE) - min(tier_num, na.rm=TRUE),
            n_added = sum(added, na.rm= TRUE),
            n_dropped = sum(dropped, na.rm= TRUE),
            n_tier_changed = length(which(!is.na(change))))

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
  filter(n_added == 0, , n_dropped == 1) %>%
  group_by(n_dropped) %>%
  summarize(count = n())

# find the number of drugs that were on the formulary in 2013, and are still on the formulary now, in the same tier.
humana_total_no_change <- humana_drug_summary %>%
  filter(n_dropped == 0 & n_added == 0 & n_tier_changed == 0) %>%
  summarize(count_no_change = n())

```

In my analysis, I found that Humana has had a total of 1,687 drugs on its formulary since 2013. Of these, over 20 percent (384) of drugs that were on the formulary in 2013 were dropped from Humana's formulary between 2013-2018. But at the same time, over 30 percent (525) of the drugs that were on the formulary in 2013 are still on the formulary now, and in the same tier.

That gave me a good snapshot of Humana's track record over the past five years. Next, I wanted to see how an individual Medicare patient would fare if they stayed with Humana from 2013-2014. I borrowed this methodology from a study Jack Hoadley co-authored called [*It Pays to Shop*](https://www.kff.org/medicare/issue-brief/it-pays-to-shop-variation-in-out-of-pocket-costs-for-medicare-part-d-enrollees-in-2016/).

I decided to use the imaginary patient I described at the top of my story — a Medicare patient in her mid-sixties taking Humalog and Humulin insulin for Type 1 diabetes, Atorvastatin for high cholesterol, and Losartan for high blood pressure.

```
#check how my imaginary Medicare patient would fare if they stayed with Humana from 
2013-2018

humana_patient <- humana_tiers_spread %>%
  filter(grepl("HUMALOG|HUMULIN|ATORVASTATIN|LOSARTAN", name, ignore.case=TRUE))
```
Here's the result:

![](https://s14.postimg.cc/lnn2c5izl/Screen_Shot_2018-05-01_at_7.58.23_PM.png)

As you can see, my imaginary patient wouldn't have had much trouble with her cholesterol and blood pressure medications, Atorvastatin and Losartan. Both stayed pretty consistent over the five year period.

But it's a different story when it comes to her diabetes medications. Humalog dropped off Humana's formulary in 2016 and Humulin moved up two tiers, making it more expensive. So, if my impaginary Medicare patient wanted to stay with Humana from 2013 to 2018, she would have had to switch insulin types, and quite possibly insulin delivery methods (not all insulins are compatable with every delivery system).

-----------
###**Next steps**

I've just scratched the surface  of this dataset. It would be interesting to classify the drugs into treatment categories like cancer treatment and anti-depressent agents to see if patients with certain diseases are disadvantaged by Humana's formulary changes. Another next step would be to track down some non-imaginary Medicare patients to ask for supporting anecdotal evidence.