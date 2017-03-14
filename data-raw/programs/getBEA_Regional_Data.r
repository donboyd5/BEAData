
# Note that as of now (11/16/2016) I am using git but have not uploaded this to the github site.

# Regional data sources
# http://www.bea.gov/regional/downloadzip.cfm


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 70, tibble.print_min = 70) # if more than x rows, print x - enough for states
# ggplot2 tibble tidyr readr purrr dplyr

library("hms") # hms, for times.
library("stringr") # stringr, for strings.
library("lubridate") # lubridate, for date/times.
library("forcats") # forcats, for factors.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")



#****************************************************************************************************
#                Globals ####
#****************************************************************************************************



#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
dq <- function(qs) {
  # convert year-q format e.g., 2005Q1 to a date, first day of quarter
  as.Date(lubridate::ymd(paste(stringr::str_sub(qs, 1, 4), as.numeric(stringr::str_sub(qs, 6)) * 3 - 2, 1)))
}

# dq("1995Q3")

#****************************************************************************************************
#                Get state annual gdp data ####
#****************************************************************************************************
# sometimes BEA calls it gross state product but I believe the formal name is state gross domestic product
# so I use gdp

# save just the state data as sgdp.a - it also has summaries by region
# starts in 1997

download.file("http://www.bea.gov/regional/zip/gsp/gsp_naics_all.zip", "./data-raw/gsp_naics_all.zip", mode="wb")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

unzip("./data-raw/gsp_naics_all.zip", list=TRUE)
# unzip("./data-raw/qgsp_all.zip", exdir=str_sub(currd, 1, -2))

df <- read_csv(unz("./data-raw/gsp_naics_all.zip", "gsp_naics_all.csv"))
glimpse(df)
count(df, GeoFIPS, GeoName, Region)

df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])
count(df2, stabbr, GeoFIPS, GeoName, Region)

df3 <- df2 %>% filter(!is.na(stabbr)) %>%
  rename(component=ComponentId,
         compname=ComponentName,
         ind=IndustryId,
         indclass=IndustryClassification,
         indname=Description) %>%
  select(-GeoFIPS, -GeoName, -Region) %>%
  gather(year, value, -stabbr, -component, -compname, -ind, -indclass, -indname)
glimpse(df3)

df4 <- df3 %>% mutate(year=as.integer(year), value=as.numeric(value)) %>%
  select(stabbr, year, everything())
glimpse(df4)
count(df4, year)
count(df4, component, compname)
count(df4, ind, indclass, indname)


# save real and nominal gdp, all industries, and then go on and save a slim file
sgdp.a_all <- df4
comment(sgdp.a_all) <- paste0("State GDP all variables, annual, downloaded ", downloaddate)
comment(sgdp.a_all)
devtools::use_data(sgdp.a_all, overwrite=TRUE)

# now save slimmed down file
df5 <- sgdp.a_all %>% filter(ind==1, component %in% c(200, 900)) %>%
  mutate(vname=ifelse(component==200, "gdp",
                      ifelse(component==900, "rgdp", "error"))) %>%
  select(stabbr, year, vname, value) %>%
  spread(vname, value)
glimpse(df5)

sgdp.a <- df5
comment(sgdp.a) <- paste0("State nominal and real GDP, annual, downloaded ", downloaddate)
comment(sgdp.a)
devtools::use_data(sgdp.a, overwrite=TRUE)

rm(df, df2, df3, df4, df5)
rm(sgdp.a, sgdp.a_all)


load("./data/sgdp.a.rda")
glimpse(sgdp.a)
comment(sgdp.a)

# load("./data/sgdp.a.rda")
# glimpse(sgdp.a)

# load("./data/sgdp.q_all.rda")
# glimpse(sgdp.q_all)


#****************************************************************************************************
#                Get state quarterly gdp data ####
#****************************************************************************************************
# save just the state data as sgdp.q - it also has summaries by region
# starts in 2005q1
ugsp <- "http://www.bea.gov/regional/zip/gsp/qgsp_all.zip"
download.file(ugsp, "./data-raw/qgsp_all.zip", mode="wb")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

unzip("./data-raw/qgsp_all.zip", list=TRUE)
# unzip("./data-raw/qgsp_all.zip", exdir=str_sub(currd, 1, -2))

df <- read_csv(unz("./data-raw/qgsp_all.zip", "qgsp_all.csv"))
glimpse(df)
count(df, GeoFIPS, GeoName, Region)

df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])
count(df2, stabbr, GeoFIPS, GeoName, Region)

df3 <- df2 %>% filter(!is.na(stabbr)) %>%
  rename(component=ComponentId,
         compname=ComponentName,
         ind=IndustryId,
         indclass=IndustryClassification,
         indname=Description) %>%
  select(-GeoFIPS, -GeoName, -Region) %>%
  gather(yearq, value, -stabbr, -component, -compname, -ind, -indclass, -indname)
glimpse(df3)

# d <- df3$yearq[1:100]

df4 <- df3 %>% mutate(date=dq(yearq), value=as.numeric(value)) %>%
  select(-yearq) %>%
  select(stabbr, date, everything())
glimpse(df4)
count(df4, component, compname)
count(df4, ind, indclass, indname)

# save real and nominal gdp, all industries, and then go on and save a slim file
sgdp.q_all <- df4
comment(sgdp.q_all) <- paste0("State GDP all variables, quarterly, downloaded ", downloaddate)
comment(sgdp.q_all)
devtools::use_data(sgdp.q_all, overwrite=TRUE)

# now save slimmed down file
df5 <- sgdp.q_all %>% filter(ind==1, component %in% c(200, 900)) %>%
  mutate(vname=ifelse(component==200, "gdp",
                      ifelse(component==900, "rgdp", "error"))) %>%
  select(stabbr, date, vname, value) %>%
  spread(vname, value)
glimpse(df5)

sgdp.q <- df5
comment(sgdp.q) <- paste0("State nominal and real GDP, quarterly, downloaded ", downloaddate)
comment(sgdp.q)
devtools::use_data(sgdp.q, overwrite=TRUE)

rm(df, df2, df3, df4, df5)
rm(sgdp.q, sgdp.q_all)

load("./data/sgdp.q.rda")


#****************************************************************************************************
#                DOWNLOAD state annual personal income data ####
#****************************************************************************************************
# http://www.bea.gov/regional/zip/spi.zip
download.file("http://www.bea.gov/regional/zip/spi.zip", "./data-raw/spi.zip", mode="wb")
downloaddate <- format(Sys.time(), '%Y-%m-%d')
unzip("./data-raw/spi.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
# tmp <- unzip("./data-raw/spi.zip", list=TRUE)


# Personal Income, Population, Per Capita Personal Income, Disposable Personal Income, and Per Capita Disposable Personal Income (SA1, SA51)
# Personal Income and Employment by Major Component (SA4)
# Personal Income by Major Component and Earnings by Industry (SA5, SA5H, SA5N)
# Compensation of Employees by Industry (SA6, SA6N)
# Wages and Salaries by Industry (SA7, SA7H, SA7N)
# Total Full-Time and Part-Time Employment by Industry (SA25, SA25N)
# Full-Time and Part-Time Wage and Salary Employment by Industry (SA27, SA27N)
# Economic Profile (SA30)
# Personal Current Transfer Receipts (SA35)
# Property Income (SA40)
# Farm Income and Expenses (SA45)
# Personal Current Taxes (SA50)


#****************************************************************************************************
#                Get SA1 state annual personal income data ####
#****************************************************************************************************
# save just the state data as spi.a - it also has summaries by region
# starts in 1997
df <- read_csv(unz("./data-raw/spi.zip", "SA1_1929_2015.csv"))
problems(df)
glimpse(df)
count(df, GeoFIPS, GeoName, Region)
count(df, IndustryClassification)
count(df, LineCode, Description)

df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(str_replace(GeoName, "\\*", ""), stcodes$stname)])
count(df2, stabbr, GeoFIPS, GeoName, Region)

df3 <- df2 %>% filter(!is.na(stabbr)) %>%
  mutate(vname=ifelse(LineCode==1, "spi",
                      ifelse(LineCode==2, "pop",
                             ifelse(LineCode==3, "pcpi", "error")))) %>%
  select(-GeoFIPS, -GeoName, -Region, -Table, -IndustryClassification, -LineCode, -Description) %>%
  gather(year, value, -stabbr, -vname) %>%
  mutate(year=as.integer(year), value=as.numeric(value)) %>%
  filter(!is.na(value))
glimpse(df3)
count(df3, vname)

df4 <- df3 %>% spread(vname, value) %>%
  mutate(pop=pop / 1000, # put in thousands
         spi=spi / 1e6) # put in billions
glimpse(df4)
count(df4, year) %>% ht
count(df4, stabbr) # includes DC and US

# save real and nominal gdp, all industries, and then go on and save a slim file
spi.a <- df4
comment(spi.a) <- paste0("State personal income ($b) and population (#k), annual, downloaded ", downloaddate)
devtools::use_data(spi.a, overwrite=TRUE)

load("./data/spi.a.rda")
glimpse(spi.a)
comment(spi.a)
spi.a %>% filter(stabbr=="NY") %>% tail(20)



#****************************************************************************************************
#                Get SA4 DETAILED COMPONENTS OF state annual personal income data ####
#****************************************************************************************************
# LineCode                                                              Description     n
# <int>                                                                    <chr> <int>
#   1        10                                   Personal income (thousands of dollars)    60
# 2        11                                               Nonfarm personal income 1/    60
# 3        12                                                           Farm income 2/    60
# 4        20                                                  Population (persons) 3/    60
# 5        30                                  Per capita personal income (dollars) 4/    60
# 6        35                                                Earnings by place of work    60
# 7        36                   Less: Contributions for government social insurance 5/    60
# 8        37 Employee and self-employed contributions for government social insurance    60
# 9        38                   Employer contributions for government social insurance    60
# 10       42                                        Plus: Adjustment for residence 6/    60
# 11       45                               Equals: Net earnings by place of residence    60
# 12       46                                   Plus: Dividends, interest, and rent 7/    60
# 13       47                                 Plus: Personal current transfer receipts    60
# 14       50                                                       Wages and salaries    60
# 15       60                                        Supplements to wages and salaries    60
# 16       61       Employer contributions for employee pension and insurance funds 8/    60
# 17       62                   Employer contributions for government social insurance    60
# 18       70                                                   Proprietors' income 9/    60
# 19       71                                                 Farm proprietors' income    60
# 20       72                                              Nonfarm proprietors' income    60
# 21     7010                                                         Total employment    60
# 22     7020                                               Wage and salary employment    60
# 23     7040                                                   Proprietors employment    60

unzip("./data-raw/spi.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)

df <- read_csv(unz("./data-raw/spi.zip", "SA4_1929_2015__ALL_AREAS.csv"))
problems(df)
glimpse(df)
names(df)[1:7]
# "GeoFIPS"  "GeoName"  "Region"        "Table"      "LineCode"     "IndustryClassification" "Description
count(df, GeoFIPS, GeoName, Region)
count(df, IndustryClassification)
count(df, LineCode, Description)

df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(str_replace(GeoName, "\\*", ""), stcodes$stname)]) %>%
  filter(!is.na(LineCode))
count(df2, stabbr, GeoFIPS, GeoName, Region)

df3 <- df2 %>% filter(!is.na(stabbr)) %>%
  select(-GeoFIPS, -GeoName, -Region, -Table, -IndustryClassification) %>%
  rename(line=LineCode, desc=Description) %>%
  gather(year, value, -stabbr, -line, -desc) %>%
  mutate(year=as.integer(year), value=as.numeric(value)) %>%
  filter(!is.na(value))
glimpse(df3)
count(df3, line, desc)
count(df3, stabbr) # includes DC and US

# save real and nominal gdp, all industries, and then go on and save a slim file
spi.a_all <- df3
downloaddate <- format(Sys.time(), '%Y-%m-%d')
comment(spi.a_all) <- paste0("State personal income DETAILS, annual, downloaded ", downloaddate)
devtools::use_data(spi.a_all, overwrite=TRUE)

load("./data/spi.a_all.rda")
glimpse(spi.a_all)
comment(spi.a_all)
spi_all.a %>% filter(stabbr=="NY") %>% tail(20)


#****************************************************************************************************
#                Get state quarterly personal income data ####
#****************************************************************************************************
# http://www.bea.gov/regional/zip/sqpi.zip
# save just the state data as spi.a - it also has summaries by region
# starts in 1997

download.file("http://www.bea.gov/regional/zip/sqpi.zip", "./data-raw/sqpi.zip", mode="wb")
unzip("./data-raw/sqpi.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
# unzip("./data-raw/sqpi.zip", exdir=str_sub(currd, 1, -2))
# file names can change - pick the correct SQ4 name

df <- read_csv(unz("./data-raw/sqpi.zip", "SQ4_1948_2016__ALL_AREAS.csv"))
problems(df)
glimpse(df)
count(df, GeoFIPS, GeoName, Region) %>% as.data.frame
count(df, IndustryClassification) # all are ...
count(df, LineCode, Description)


# df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(str_replace(GeoName, "\\*", ""), stcodes$stname)])
df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(str_sub(GeoFIPS, 1, 2), stcodes$stfips)])
count(df2, stabbr, GeoFIPS, GeoName, Region)

df3 <- df2 %>% filter(!is.na(stabbr)) %>%
  rename(line=LineCode,
         description=Description) %>%
  select(-GeoFIPS, -GeoName, -Region, -Table, -IndustryClassification) %>%
  gather(yearq, value, -stabbr, -line, -description) %>%
  mutate(value=as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  mutate(date=dq(yearq))

glimpse(df3)
count(df3, yearq, date)
count(df3, line, description)
count(df3, stabbr) # includes DC and US

# add vnames
vnames <- read_csv("line, vname
10, spi
11, nonfarmpi
12, farmpi
20, pop
30, pcpi
35, earn.pow
36, c.socins
37, eec.socins
38, erc.socins
42, resadj
45, netearn.por
46, divintrent
47, perstransf
50, wages
60, wagesups
61, erc.pension
62, erc.socins2
70, propinc
71, farmprop.inc
72, nonfarm.propinc")
vnames %>% left_join(count(df3, line, description))


df4 <- df3 %>% left_join(vnames) %>%
  select(stabbr, date, line, vname, description, value)
glimpse(df4)

df4 %>% select(stabbr, date, vname, value) %>% spread(vname, value)

# save
downloaddate <- format(Sys.time(), '%Y-%m-%d')

spi.q <- df4
comment(spi.q) <- paste0("State personal income components and selected other variables, quarterly, downloaded ", downloaddate)
devtools::use_data(spi.q, overwrite=TRUE)

spiw.q <- spi.q %>% select(stabbr, date, vname, value) %>%
  spread(vname, value)
glimpse(spiw.q)
comment(spiw.q) <- paste0("State personal income components and selected other variables, wide, quarterly, downloaded ",
                          downloaddate)
devtools::use_data(spiw.q, overwrite=TRUE)


load("./data/spi.q.rda")
glimpse(spi.q)
comment(spi.q)
spi.q %>% filter(stabbr=="NY") %>% tail(20)


#****************************************************************************************************
#                Get state annual personal consumption expenditure data ####
#****************************************************************************************************
# http://www.bea.gov/regional/zip/PCEbyState.zip
# save just the state data as spi.a - it also has summaries by region
# starts in 1997

download.file("http://www.bea.gov/regional/zip/PCEbyState.zip", "./data-raw/PCEbyState.zip", mode="wb")
unzip("./data-raw/PCEbyState.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
# unzip("./data-raw/PCEbyState.zip", exdir=str_sub(currd, 1, -2))

df <- read_csv(unz("./data-raw/PCEbyState.zip", "PCE_all.csv"))
problems(df)
glimpse(df)
count(df, GeoFIPS, GeoName, Region)
count(df, ComponentId, ComponentName) # all items are nominal PCE
count(df, IndustryClassification) # all are ...
count(df, Line, Description)

df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(str_replace(GeoName, "\\*", ""), stcodes$stname)])
count(df2, stabbr, GeoFIPS, GeoName, Region)

df3 <- df2 %>% filter(!is.na(stabbr)) %>%
  rename(line=Line, description=Description) %>%
  select(-c(GeoFIPS, GeoName, Region, ComponentId, ComponentName, IndustryClassification)) %>%
  gather(year, value, -stabbr, -line, -description) %>%
  mutate(year=as.integer(year), value=as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  mutate(value=value / 1000) %>% # put in $ billions
  select(stabbr, year, line, description, value)
glimpse(df3)
count(df3, line, description)
count(df3, year) %>% ht
count(df3, stabbr) # includes DC and US

# save real and nominal gdp, all industries, and then go on and save a slim file
spce.a <- df3
downloaddate <- format(Sys.time(), '%Y-%m-%d')
comment(spce.a) <- paste0("State personal consumption expenditures ($b), annual, downloaded ", downloaddate)
devtools::use_data(spce.a, overwrite=TRUE)

load("./data/spce.a.rda")
glimpse(spce.a)
comment(spce.a)
spce.a %>% filter(stabbr=="NY") %>% tail(20)



#****************************************************************************************************
#                Review package ####
#****************************************************************************************************
data(package="BEAData")

data(package="bdata") # remove spi.a, spi.q from this when updated - use BEAData from now on (11/16/2016)

