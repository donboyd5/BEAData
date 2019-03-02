# 7/15/2017 djb Need to fix state quarterly personal income and personal consumption.

# Note that as of now (11/16/2016) I am using git but have not uploaded this to the github site.

# Regional data sources
# http://www.bea.gov/regional/downloadzip.cfm
# https://www.bea.gov/data/economic-accounts/regional

# For links to documentation see:
#   https://apps.bea.gov/regional/quick.cfm

# API
#   https://apps.bea.gov/api/_pdf/bea_web_service_api_user_guide.pdf


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

dq2 <- function(qs) {
  # convert year-q format e.g., 2005:Q1 to a date, first day of quarter
  as.Date(lubridate::ymd(paste(stringr::str_sub(qs, 1, 4), as.numeric(stringr::str_sub(qs, 7)) * 3 - 2, 1)))
}

# dq("1995Q3")

get_gdp <- function(fn, vname){
  df <- read_delim(unz("./data-raw/SAGDP.zip", fn),
                   delim=",",
                   escape_double = FALSE,
                   col_types = cols(GeoFIPS=col_character(),
                                    GeoName=col_character(),
                                    TableName=col_character(),
                                    ComponentName=col_character(),
                                    Unit=col_character(),
                                    IndustryClassification=col_character(),
                                    Description=col_character(),
                                    .default= col_double()))

  df2 <- df %>%
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"),
           GeoName=str_remove(GeoName, "[*]+"),
           stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])

  df3 <- df2 %>% filter(!is.na(stabbr)) %>%
    rename(ind=IndustryId,
           indclass=IndustryClassification,
           indname=Description) %>%
    select(-GeoFIPS, -GeoName, -Region, -TableName, -ComponentName, -Unit) %>%
    gather(year, value, -stabbr, -ind, -indclass, -indname)

  df4 <- df3 %>%
    mutate(vname=vname,
           year=as.integer(year), value=as.numeric(value)) %>%
    select(vname, stabbr, year, everything())

  return(df4)
}


get_pce <- function(fn, vname){
  # GeoFIPS	GeoName	Region	TableName	ComponentName	Unit	Line	IndustryClassification	Description
  df <- read_delim(unz("./data-raw/SAEXP.zip", fn),
                   delim=",",
                   escape_double = FALSE,
                   col_types = cols(GeoFIPS=col_character(),
                                    GeoName=col_character(),
                                    TableName=col_character(),
                                    ComponentName=col_character(),
                                    Unit=col_character(),
                                    IndustryClassification=col_character(),
                                    Description=col_character(),
                                    .default= col_double()))

  df2 <- df %>%
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"),
           GeoName=str_remove(GeoName, "[*]+"),
           stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])

  df3 <- df2 %>% filter(!is.na(stabbr)) %>%
    rename(line=Line,
           pcename=Description) %>%
    select(-GeoFIPS, -GeoName, -Region, -TableName, -ComponentName, -Unit, -IndustryClassification) %>%
    gather(year, value, -stabbr, -line, -pcename)

  df4 <- df3 %>%
    mutate(vname=vname,
           year=as.integer(year), value=as.numeric(value)) %>%
    select(vname, stabbr, year, everything())

  return(df4)
}



#****************************************************************************************************
#                Temporary download workarounds ####
#****************************************************************************************************
# Download links don't seem to work, so:
# Go to: https://apps.bea.gov/regional/downloadzip.cfm
#   download needed files
#     copy to D:\Dropbox\RPrograms PC\Packages\BEAData\data-raw


# See api info for info on file names
# https://apps.bea.gov/regional/docs/RegionalApi.cfm

# SAGDP10S__ALL_AREAS_1977_1997.csv  Per capita real GDP by state	Chained 1997 dollars

# SAGDP2N	Gross domestic product (GDP) by state	Millions of current dollars SAGDP2N__ALL_AREAS_1997_2017.csv
# SAGDP3N	Taxes on production and imports less subsidies	Thousands of dollars
# SAGDP4N	Compensation of employees	Thousands of dollars
# SAGDP5N	Subsidies	Thousands of dollars
# SAGDP6N	Taxes on production and imports	Thousands of dollars
# SAGDP7N	Gross operating surplus	Thousands of dollars
# SAGDP8N	Quantity indexes for real GDP by state (2012=100.0)
# SAGDP9N__ALL_AREAS_1997_2017.csv


#****************************************************************************************************
#                Get state annual gdp data ####
#****************************************************************************************************
# sometimes BEA calls it gross state product but I believe the formal name is state gross domestic product
# so I use gdp

# save just the state data as sgdp.a - it also has summaries by region
# starts in 1997

# download.file("http://www.bea.gov/regional/zip/gsp/gsp_naics_all.zip", "./data-raw/gsp_naics_all.zip", mode="wb")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

# get data

gdp <- get_gdp("SAGDP2N__ALL_AREAS_1997_2017.csv", "gdp")
rgdp <- get_gdp("SAGDP9N__ALL_AREAS_1997_2017.csv", "rgdp")

gdp.all <- bind_rows(gdp, rgdp)
ht(gdp.all)


# save gdp, all industries, and then go on and save a slim file
sgdp.a_all <- gdp.all
comment(sgdp.a_all) <- paste0("State GDP, nominal and real, all variables, annual, downloaded ", downloaddate)
comment(sgdp.a_all)
usethis::use_data(sgdp.a_all, overwrite=TRUE)

# now save slimmed down file
dfslim <- sgdp.a_all %>% filter(ind==1) %>%
  select(stabbr, year, vname, value) %>%
  spread(vname, value)
glimpse(dfslim)

sgdp.a <- dfslim
comment(sgdp.a) <- paste0("State nominal and real GDP, annual, downloaded ", downloaddate)
comment(sgdp.a)
usethis::use_data(sgdp.a, overwrite=TRUE)

rm(sgdp.a, sgdp.a_all)


load("./data/sgdp.a.rda")
glimpse(sgdp.a)
comment(sgdp.a)

# load("./data/sgdp.a.rda")
# glimpse(sgdp.a)

# load("./data/sgdp.q_all.rda")
# glimpse(sgdp.q_all)


#****************************************************************************************************
#                TODO as of 3/2/2019: Get state quarterly gdp data ####
#****************************************************************************************************
# save just the state data as sgdp.q - it also has summaries by region
# starts in 2005q1
# ugsp <- "http://www.bea.gov/regional/zip/gsp/qgsp_all.zip"
# download.file(ugsp, "./data-raw/qgsp_all.zip", mode="wb")
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
# download.file("http://www.bea.gov/regional/zip/spi.zip", "./data-raw/spi.zip", mode="wb")

# unzip("./data-raw/spi.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
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
downloaddate <- format(Sys.time(), '%Y-%m-%d')

unzip("./data-raw/SAINC.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
unzip("./data-raw/SAINC.zip", list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS")) %>% arrange(Name)

# From xml definitions files:
# SAINC1" Name="Personal Income Summary: Personal Income, Population, Per Capita Personal Income SAINC1_1998_2017_ALL_AREAS.csv
# SAINC4" Name="Personal Income and Employment by Major Component SAINC4_1998_2017_ALL_AREAS.csv
# SAINC5N" Name="Personal Income by Major Component and Earnings by NAICS Industry
# SAINC6N" Name="Compensation of Employees by NAICS Industry
# SAINC7N" Name="Wages and Salaries by NAICS Industry
# SAINC30" Name="Economic Profile  SAINC30_1998_2017_ALL_AREAS.csv
# SAINC35" Name="Personal Current Transfer Receipts SAINC35_1998_2017_ALL_AREAS.csv
# SAINC40" Name="Property Income SAINC40_1998_2017_ALL_AREAS.csv
# SAINC45" Name="Farm Income and Expenses SAINC45_1998_2017_ALL_AREAS.csv
# SAINC50" Name="Personal Current Taxes SAINC50_1998_2017_ALL_AREAS.csv
# SAINC51" Name="Disposable Personal Income Summary: Disposable Personal Income, Population, and Per Capita Disposable Personal Income SAINC51_1998_2017_ALL_AREAS.csv

# SAEMP27N" Name="Full-Time and Part-Time Wage and Salary Employment by NAICS Industry
fn <- "SAINC1_1998_2017_ALL_AREAS.csv"

get_spi <- function(fn, vname){
  # GeoFIPS	GeoName	Region	TableName	LineCode	IndustryClassification	Description	Unit
  df <- read_delim(unz("./data-raw/SAINC.zip", fn),
                   delim=",",
                   escape_double = FALSE,
                   col_types = cols(GeoFIPS=col_character(),
                                    GeoName=col_character(),
                                    TableName=col_character(),
                                    IndustryClassification=col_character(),
                                    Description=col_character(),
                                    Unit=col_character(),
                                    .default= col_double()))

  df2 <- df %>%
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"),
           GeoName=str_remove(GeoName, "[*]+"),
           stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])

  df3 <- df2 %>% filter(!is.na(stabbr)) %>%
    rename(line=LineCode,
           spiname=Description) %>%
    filter(line==1) %>%
    select(-GeoFIPS, -GeoName, -Region, -TableName, -line, -IndustryClassification, -Unit) %>%
    gather(year, value, -stabbr, -spiname)

  df4 <- df3 %>%
    mutate(vname=vname,
           year=as.integer(year),
           value=as.numeric(value)) %>%
    select(vname, stabbr, year, everything())

  return(df4)
}


# save just the state data as spi.a - it also has summaries by region
spi.a <- get_spi("SAINC1_1998_2017_ALL_AREAS.csv", "spi")
ht(spi.a)

comment(spi.a) <- paste0("State personal income ($b) and population (#k), annual, downloaded ", downloaddate)
usethis::use_data(spi.a, overwrite=TRUE)

load("./data/spi.a.rda")
glimpse(spi.a)
comment(spi.a)
spi.a %>% filter(stabbr=="NY") %>% tail(20)


#****************************************************************************************************
#                TODO as of 3/2/2019: Get SA4 DETAILED COMPONENTS OF state annual personal income data ####
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

df <- read_csv(unz("./data-raw/spi.zip", "SA4_1929_2016__ALL_AREAS.csv"))
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
usethis::use_data(spi.a_all, overwrite=TRUE)

load("./data/spi.a_all.rda")
glimpse(spi.a_all)
comment(spi.a_all)
spi.a_all %>% filter(stabbr=="NY") %>% tail(20)


#****************************************************************************************************
#                TODO as of 3/2/2019: Get state quarterly personal income data ####
#****************************************************************************************************
# http://www.bea.gov/regional/zip/sqpi.zip
# save just the state data as spi.a - it also has summaries by region
# starts in 1997

download.file("http://www.bea.gov/regional/zip/sqpi.zip", "./data-raw/sqpi.zip", mode="wb")
unzip("./data-raw/sqpi.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
# unzip("./data-raw/sqpi.zip", exdir=str_sub(currd, 1, -2))
# file names can change - pick the correct SQ4 name

df <- read_csv(unz("./data-raw/sqpi.zip", "SQ4_1948_2017__ALL_AREAS.csv"))
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
  mutate(date=dq2(yearq))

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
usethis::use_data(spiw.q, overwrite=TRUE)


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

# download.file("http://www.bea.gov/regional/zip/PCEbyState.zip", "./data-raw/PCEbyState.zip", mode="wb")
unzip("./data-raw/SAEXP.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
downloaddate <- format(Sys.time(), '%Y-%m-%d')

# SAEXP1_1997_2017_ALL_AREAS_.csv  Total personal consumption expenditures (PCE) by state	Millions of current dollars
# SAEXP2_1997_2017_ALL_AREAS_.csv SAEXP2	Per capita personal consumption expenditures (PCE) by state	Dollars

pce <- get_pce("SAEXP1_1997_2017_ALL_AREAS_.csv", "pce")
glimpse(pce)
ht(pce)

spce.a <- pce

comment(spce.a) <- paste0("State personal consumption expenditures ($m), annual, downloaded ", downloaddate)
usethis::use_data(spce.a, overwrite=TRUE)

load("./data/spce.a.rda")
glimpse(spce.a)
comment(spce.a)
spce.a %>% filter(stabbr=="NY") %>% ht(15)


#****************************************************************************************************
#                Review package ####
#****************************************************************************************************
data(package="BEAData")

data(package="bdata") # remove spi.a, spi.q from this when updated - use BEAData from now on (11/16/2016)

