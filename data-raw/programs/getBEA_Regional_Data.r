# 3/3/2019

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
bdir <- "D:/Data/BEAData/" # put any manually downloaded files here

rgnfiles <- c("SAGDP", "SQGDP", "SAINC", "SQINC", "SARPP", "SAEXP") # SAGDP SQGDP SAINC SQINC SARPP SAEXP


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

download.rzip <- function(rfn){
  # download a regional zip file from https://apps.bea.gov/regional/zip
  # names include: SAGDP SQGDP SAINC SQINC SARPP SAEXP
  urlbase <- "https://apps.bea.gov/regional/zip/"
  download.file(paste0(urlbase, rfn, ".zip"), paste0(bdir, rfn, ".zip"), mode="wb")
}


#****************************************************************************************************
#                Download info ####
#****************************************************************************************************
# Go to: https://apps.bea.gov/regional/downloadzip.cfm to see available files
# See api info for info on file names
# https://apps.bea.gov/regional/docs/RegionalApi.cfm

# Example URL:
#   https://apps.bea.gov/regional/zip/SQGDP.zip
#   Files are: "SAGDP" "SQGDP" "SAINC" "SQINC" "SARPP" "SAEXP"


#****************************************************************************************************
#                Get state annual gdp data ####
#****************************************************************************************************
# save just the state data as sgdp.a - it also has summaries by region; starts in 1997
get_gdp <- function(fn, vname){
  fullpath <- paste0(bdir, "SAGDP.zip")
  df <- read_delim(unz(fullpath, fn),
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


download.rzip("SAGDP")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

# peek at the files as needed
fn <- paste0(bdir, "SAGDP.zip")
(fnames <- unzip(fn, list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS")) %>% .[["Name"]] %>% sort)
read_csv(unz(fn, fnames[4]), n_max=10) %>% select(c(1:10, ncol(.))) %>% as.data.frame

# SAGDP10S__ALL_AREAS_1977_1997.csv  Per capita real GDP by state	Chained 1997 dollars

# SAGDP2N	Gross domestic product (GDP) by state	Millions of current dollars SAGDP2N__ALL_AREAS_1997_2017.csv
# SAGDP3N	Taxes on production and imports less subsidies	Thousands of dollars
# SAGDP4N	Compensation of employees	Thousands of dollars
# SAGDP5N	Subsidies	Thousands of dollars
# SAGDP6N	Taxes on production and imports	Thousands of dollars
# SAGDP7N	Gross operating surplus	Thousands of dollars
# SAGDP8N	Quantity indexes for real GDP by state (2012=100.0)
# SAGDP9N__ALL_AREAS_1997_2017.csv
# end peek

gdp <- get_gdp("SAGDP2N__ALL_AREAS_1997_2018.csv", "gdp")
rgdp <- get_gdp("SAGDP9N__ALL_AREAS_1997_2018.csv", "rgdp")

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


#****************************************************************************************************
#                Get state quarterly gdp data ####
#****************************************************************************************************

get_gdpq <- function(fn, vname){
  fullpath <- paste0(bdir, "SQGDP.zip")
  df <- read_delim(unz(fullpath, fn),
                   delim=",",
                   escape_double = FALSE,
                   col_types = cols(GeoFIPS=col_character(),
                                    GeoName=col_character(),
                                    Region=col_double(),
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
    gather(yearq, value, -stabbr, -ind, -indclass, -indname)

  df4 <- df3 %>%
    mutate(vname=vname,
           date=dq2(yearq),
           value=as.numeric(value)) %>%
    select(vname, stabbr, date, ind, indclass, indname, value)

  return(df4)
}

# save just the state data as sgdp.q - it also has summaries by region
# starts in 2005q1
download.rzip("SQGDP")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

zfn <- paste0(bdir, "SQGDP.zip")
unzip(zfn, list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS"))
(fnames <- unzip(zfn, list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS")) %>% .[["Name"]] %>% sort)

# peek at the files as needed
read_csv(unz(zfn, fnames[2]), n_max=10) %>% select(c(1:10, ncol(.))) %>% as.data.frame

# SQGDP11__ALL_AREAS_2005_2018.csv Contributions to percent change in real GDP Percentage points
# SQGDP2__ALL_AREAS_2005_2018.csv Gross domestic product (GDP) by state Millions of current dollars
# SQGDP8__ALL_AREAS_2005_2018.csv Quantity indexes for real GDP by state (2012=100.0)
# SQGDP9__ALL_AREAS_2005_2018.csv Real GDP by state Millions of chained 2012 dollars
# all run from 2005:Q1 (using this date format) to latest
# end peek

ngdpq <- get_gdpq("SQGDP2__ALL_AREAS_2005_2019.csv", "gdp")
ht(ngdpq)

rgdpq <- get_gdpq("SQGDP9__ALL_AREAS_2005_2019.csv", "rgdp")
ht(rgdpq)

# save real and nominal gdp, all industries, and then go on and save a slim file
sgdp.q_all <- bind_rows(ngdpq, rgdpq)
comment(sgdp.q_all) <- paste0("State GDP all variables, quarterly, downloaded ", downloaddate)
comment(sgdp.q_all)
usethis::use_data(sgdp.q_all, overwrite=TRUE)

# now save slimmed down file
dfslim <- sgdp.q_all %>%
  filter(ind==1,) %>%
  select(stabbr, date, vname, value) %>%
  spread(vname, value)
glimpse(dfslim)
ht(dfslim)

sgdp.q <- dfslim
comment(sgdp.q) <- paste0("State nominal and real GDP, quarterly, downloaded ", downloaddate)
comment(sgdp.q)
usethis::use_data(sgdp.q, overwrite=TRUE)

rm(sgdp.q, sgdp.q_all)
load("./data/sgdp.q.rda")


#****************************************************************************************************
#                Get SAINC1 state annual personal income data ####
#****************************************************************************************************
get_spi <- function(fn, vname, lines=NULL){
  # GeoFIPS	GeoName	Region	TableName	LineCode	IndustryClassification	Description	Unit
  # lines is an integer vector of which lines to keep -- NULL means ALL
  fullpath <- paste0(bdir, "SAINC.zip")
  df <- read_delim(unz(fullpath, fn),
                   delim=",",
                   escape_double = FALSE,
                   col_types = cols(GeoFIPS=col_character(),
                                    GeoName=col_character(),
                                    TableName=col_character(),
                                    IndustryClassification=col_character(),
                                    Description=col_character(),
                                    Unit=col_character(),
                                    .default= col_double()))

  if(is.null(lines)) lines <- unique(df$LineCode)

  df2 <- df %>%
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"),
           GeoName=str_remove(GeoName, "[*]+"),
           stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])

  df3 <- df2 %>% filter(!is.na(stabbr)) %>%
    rename(line=LineCode,
           spiname=Description) %>%
    filter(line %in% lines) %>%
    select(-GeoFIPS, -GeoName, -Region, -TableName, -IndustryClassification, -Unit) %>%
    gather(year, value, -stabbr, -spiname, -line)

  df4 <- df3 %>%
    mutate(vname=vname,
           year=as.integer(year),
           value=as.numeric(value)) %>%
    select(vname, stabbr, year, everything())

  return(df4)
}


download.rzip("SAINC")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

fn <- paste0(bdir, "SAINC.zip")

unzip(fn, list=TRUE) %>% arrange(desc(Length)) %>% head(20)
unzip(fn, list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS")) %>% arrange(Name)

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

# save just the state data as spi.a - it also has summaries by region
spi.a <- get_spi("SAINC1_1998_2018_ALL_AREAS.csv", "spi", lines=NULL)
# spi.a <- get_spi("SAINC1__ALL_AREAS_1929_2018.csv", "spi", lines=NULL)
ht(spi.a)

comment(spi.a) <- paste0("State personal income ($m), population (#), pci ($), annual, downloaded ", downloaddate)
usethis::use_data(spi.a, overwrite=TRUE)

load("./data/spi.a.rda")
glimpse(spi.a)
comment(spi.a)
spi.a %>% filter(stabbr=="NY") %>% tail(20)


#****************************************************************************************************
#                Get SAINC4 DETAILED COMPONENTS OF state annual personal income data ####
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

spi.a_all <- get_spi("SAINC4_1998_2017_ALL_AREAS.csv", "spi")
ht(spi.a_all)
count(spi.a_all, line, spiname)
comment(spi.a_all) <- paste0("State personal income DETAILS, annual, downloaded ", downloaddate)
usethis::use_data(spi.a_all, overwrite=TRUE)

load("./data/spi.a_all.rda")
glimpse(spi.a_all)
comment(spi.a_all)
spi.a_all %>% filter(stabbr=="NY") %>% tail(20)


#****************************************************************************************************
#                Get state quarterly personal income data ####
#****************************************************************************************************
fn <- "SQINC1__ALL_AREAS_1998_2018.csv"
vname <- "spi"
get_spiq <- function(fn, vname){
  fullpath <- paste0(bdir, "SQINC.zip")
  df <- read_delim(unz(fullpath, fn),
                   delim=",",
                   escape_double = FALSE,
                   col_types = cols(GeoFIPS=col_character(),
                                    GeoName=col_character(),
                                    Region=col_double(),
                                    TableName=col_character(),
                                    LineCode=col_integer(),
                                    IndustryClassification=col_character(),
                                    Description=col_character(),
                                    Unit=col_character(),
                                    .default= col_double()))

  df2 <- df %>%
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"),
           GeoName=str_remove(GeoName, "[*]+"),
           stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])

  df3 <- df2 %>%
    filter(!is.na(stabbr)) %>%
    rename(line=LineCode,
           description=Description) %>%
    select(-GeoFIPS, -GeoName, -Region, -TableName, -IndustryClassification, -Unit) %>%
    gather(yearq, value, -stabbr, -description, -line) %>%
    filter(!is.na(value)) # a lot of missing data in early years

  df4 <- df3 %>%
    mutate(vname=vname,
           date=dq2(yearq),
           value=as.numeric(value)) %>%
    select(vname, stabbr, date, line, description, value)

  return(df4)
}


# save just the state data as sgdp.q - it also has summaries by region
# starts in 2005q1
download.rzip("SQINC")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

zfn <- paste0(bdir, "SQINC.zip")
unzip(zfn, list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS"))
(fnames <- unzip(zfn, list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS")) %>% .[["Name"]] %>% sort)

# peek at the files as needed
read_csv(unz(zfn, fnames[1]), n_max=10) %>% select(c(1:9, ncol(.))) %>% as.data.frame

# Files guess based on annual files
# SQINC1" Name="Personal Income Summary: Personal Income, Population, Per Capita Personal Income SQINC1__ALL_AREAS_1998_2018.csv
# SQINC35" Name="Personal Current Transfer Receipts SQINC35_1998_2018_ALL_AREAS.csv
# SQINC4" Name="Personal Income and Employment by Major Component SQINC4__ALL_AREAS_1998_2018.csv
# SQINC5N" Name="Personal Income by Major Component and Earnings by NAICS Industry SQINC5N__ALL_AREAS_1998_2018.csv
# SQINC6N" Name="Compensation of Employees by NAICS Industry SQINC6N__ALL_AREAS_1998_2018.csv
# SQINC7N" Name="Wages and Salaries by NAICS Industry SQINC7N__ALL_AREAS_1998_2018.csv
# all run from 1998:Q1 (using this date format) to latest
# end peek

df <- get_spiq("SQINC4__ALL_AREAS_1998_2018.csv", "spi")
ht(df)
count(df, line, description)

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
vnames %>% left_join(count(df, line, description))

df2 <- df %>%
  select(-vname) %>%
  left_join(vnames) %>%
  select(stabbr, date, line, vname, description, value)
glimpse(df2)

df2 %>% select(stabbr, date, vname, value) %>% spread(vname, value)

# save
spi.q <- df2
comment(spi.q) <- paste0("State personal income components and selected other variables, quarterly, downloaded ", downloaddate)
usethis::use_data(spi.q, overwrite=TRUE)

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
get_pce <- function(fn, vname){
  # GeoFIPS	GeoName	Region	TableName	ComponentName	Unit	Line	IndustryClassification	Description
  fullpath <- paste0(bdir, "SAEXP.zip")
  df <- read_delim(unz(fullpath, fn),
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


# http://www.bea.gov/regional/zip/PCEbyState.zip
download.rzip("SAEXP")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

fn <- paste0(bdir, "SAEXP.zip")

unzip(fn, list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS")) %>% arrange(desc(Length))

# SAEXP1_1997_2017_ALL_AREAS_.csv  Total personal consumption expenditures (PCE) by state	Millions of current dollars
# SAEXP2_1997_2017_ALL_AREAS_.csv SAEXP2	Per capita personal consumption expenditures (PCE) by state	Dollars

spce.a <- get_pce("SAEXP1_1997_2017_ALL_AREAS_.csv", "pce")
glimpse(spce.a)
ht(spce.a)

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

