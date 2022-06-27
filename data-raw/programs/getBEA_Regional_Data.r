# 6/27/2022
# TODO: update state PCE

# Regional data sources
# http://www.bea.gov/regional/downloadzip.cfm
# https://www.bea.gov/data/economic-accounts/regional

# For links to documentation see:
#   https://apps.bea.gov/regional/quick.cfm

# API
#   https://apps.bea.gov/api/_pdf/bea_web_service_api_user_guide.pdf


# libraries ---------------------------------------------------------------

# library(conflicted)
library(magrittr)
# library(plyr) # needed for ldply; must be loaded BEFORE dplyr
library(tidyverse)
options(tibble.print_max = 70, tibble.print_min = 70) # if more than x rows, print x - enough for states
# ggplot2 tibble tidyr readr purrr dplyr

library(hms) # hms, for times.
library(stringr) # stringr, for strings.
library(lubridate) # lubridate, for date/times.
library(forcats) # forcats, for factors.
library(readxl) # readxl, for .xls and .xlsx files.
library(haven) # haven, for SPSS, SAS and Stata files.

library(grDevices)
library(knitr)

library(zoo) # for rollapply

library(btools) # library that I created (install from github)
library(bdata)


# resolve conflicts -------------------------------------------------------
# conflict_prefer("filter", "dplyr")
tidyverse_conflicts()


# globals -----------------------------------------------------------------

bdir <- "E:/data/BEAData/" # put any manually downloaded files here
rgnfiles <- c("SAGDP", "SQGDP", "SAINC", "SQINC", "SARPP", "SAEXP") # SAGDP SQGDP SAINC SQINC SARPP SAEXP


# functions ---------------------------------------------------------------

dq <- function(qs) {
  # convert year-q format e.g., 2005Q1 to a date, first day of quarter
  # dq("1995Q3")
  as.Date(lubridate::ymd(paste(stringr::str_sub(qs, 1, 4), as.numeric(stringr::str_sub(qs, 6)) * 3 - 2, 1)))
}

dq2 <- function(qs) {
  # convert year-q format e.g., 2005:Q1 to a date, first day of quarter
  as.Date(lubridate::ymd(paste(stringr::str_sub(qs, 1, 4), as.numeric(stringr::str_sub(qs, 7)) * 3 - 2, 1)))
}

download.rzip <- function(rfn){
  # download a regional zip file from https://apps.bea.gov/regional/zip
  # names include: SAGDP SQGDP SAINC SQINC SARPP SAEXP
  urlbase <- "https://apps.bea.gov/regional/zip/"
  download.file(paste0(urlbase, rfn, ".zip"), paste0(bdir, rfn, ".zip"), mode="wb")
}


# notes on download info -----------------------------------------------------------
# Go to: https://apps.bea.gov/regional/downloadzip.cfm to see available files
# See api info for info on file names
# https://apps.bea.gov/regional/docs/RegionalApi.cfm

# Example URL:
#   https://apps.bea.gov/regional/zip/SQGDP.zip
#   Files are: "SAGDP" "SQGDP" "SAINC" "SQINC" "SARPP" "SAEXP"


# Get state annual gdp data on NAICS basis ---------------------------------

# save just the state data as sgdp.a - it also has summaries by region; starts in 1997
# we want SAGDP2N Gross domestic product (GDP) by state, which is NAICS basis (the "N")

# fn <- "SAGDP2N__ALL_AREAS_1997_2019.csv"
# vname <- "gdp"

get_gdp2 <- function(fn, vname){
  # new format in 2020?
  # fn <- "SAGDP2N__ALL_AREAS_1997_2020.csv"
  # vname <- "gdp"
  fullpath <- paste0(bdir, "SAGDP.zip")
  df <- read_delim(unz(fullpath, fn),
                   delim=",",
                   escape_double = FALSE,
                   col_types = cols(GeoFIPS=col_character(),
                                    GeoName=col_character(),
                                    IndustryClassification=col_character(),
                                    Region=col_character(),
                                    TableName=col_character(),
                                    Unit=col_character(),
                                    Description=col_character(),
                                    .default= col_double()))

  df2 <- df %>%
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"), # get rid of nonnumerics such as \,"
           GeoName=str_remove(GeoName, "[*]+") %>% str_trim, # remove asterisks
           stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])

  df3 <- df2 %>% filter(!is.na(stabbr)) %>%
    dplyr::rename(line=LineCode,
           indclass=IndustryClassification,
           indname=Description) %>%
    select(-GeoFIPS, -GeoName, -Region, -TableName, -Unit) %>%
    pivot_longer(-c(stabbr, line, indclass, indname), names_to = "year")

  df4 <- df3 %>%
    mutate(vname=vname,
           year=as.integer(year),
           value=as.numeric(value)) %>%
    select(vname, stabbr, year, everything())

  return(df4)
}

download.rzip("SAGDP")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

# peek at the files as needed
fn <- paste0(bdir, "SAGDP.zip")
# find the
# SAGDP2N NAICS file
(fnames <- unzip(fn, list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS")) %>% .[["Name"]] %>% sort)
(fname <- str_subset(fnames, "SAGDP2N"))

read_csv(unz(fn, fname), n_max=10) %>%
  select(c(1:10, ncol(.))) %>%
  as_tibble

colinfo <- read_csv(unz(fn, fnames[4]), n_max=1)
colinfo

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

glimpse(colinfo)
gdp <- get_gdp2(fname, "gdp")
rgdp <- get_gdp2(fname, "rgdp")

gdp.all <- bind_rows(gdp, rgdp)
ht(gdp.all)

# save gdp, all industries, and then go on and save a slim file
sgdp.a_all <- gdp.all
comment(sgdp.a_all) <- paste0("State GDP, nominal and real, all variables, annual, downloaded ", downloaddate)
comment(sgdp.a_all)
usethis::use_data(sgdp.a_all, overwrite=TRUE)

#.. now save slimmed down file ----
dfslim <- sgdp.a_all %>% filter(line==1) %>%
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


# Get state annual GDP on SIC basis ---------------------------------------
summary(sgdp.a)  # 1997-2020
# get the SIC-basis data
# urlbase <- "https://apps.bea.gov/regional/zip/"
# SAGDP_SIC.zip
# SAGDP2S Millions of current dollars
# SAGDP9S Millions of chained 1997 dollars
# download.file(paste0(urlbase, rfn, ".zip"), paste0(bdir, rfn, ".zip"), mode="wb")

download.rzip("SAGDP_SIC")

fn <- paste0(bdir, "SAGDP_SIC.zip")
(fnames <- unzip(fn, list=TRUE) %>%
    filter(str_detect(Name, "ALL_AREAS")) %>%
    pull(Name) %>%
    sort)
# 1963-1977

(fname_nom <- fnames %>% str_subset("SAGDP2S"))
(fname_real <- fnames %>% str_subset("SAGDP9S"))

read_csv(unz(fn, fname_nom), n_max=10) %>%
  select(c(1:10, ncol(.))) %>%
  as_tibble # nominal gdp $ millions

read_csv(unz(fn, fname_real), n_max=10) %>%
  select(c(1:10, ncol(.))) %>%
  as_tibble # real gdp $ millions chained 1997

dfnom <- read_csv(unz(fn, fname_nom)) # $ millions
dfreal <- read_csv(unz(fn, fname_real)) # $ Millions of chained 1997 dollars
glimpse(dfnom)
glimpse(dfreal)

idvars <- c("GeoFIPS", "GeoName", "Region", "TableName", "LineCode", "IndustryClassification", "Description", "Unit")
dfnom2 <- dfnom %>%
  filter(LineCode==1, GeoFIPS < "90000") %>%
  pivot_longer(cols=-all_of(idvars), names_to = "year") %>%
  mutate(vname="gdp")

dfreal2 <- dfreal %>%
  filter(LineCode==1, GeoFIPS < "90000") %>%
  pivot_longer(cols=-all_of(idvars), names_to = "year") %>%
  mutate(vname="rgdp")

df <- bind_rows(dfnom2, dfreal2) %>%
  mutate(stfips=str_sub(GeoFIPS, 1, 2)) %>%
  left_join(stcodes %>% select(stfips, stabbr), by="stfips")

count(df, TableName, Unit) # prefix dplyr:: if conflicts
count(df, stabbr, GeoName) # 50 states, DC, US
count(df, year) # 1963-1977

sgdp_sic.a <- df %>%
  select(stabbr, year, vname, value) %>%
  mutate(year=as.integer(year),
         value=as.numeric(value)) %>%
  pivot_wider(names_from=vname)

comment(sgdp_sic.a) <- paste0("State nominal and real GDP SIC basis, annual, downloaded ", downloaddate)
comment(sgdp_sic.a)
usethis::use_data(sgdp_sic.a, overwrite=TRUE)


# Splice state annual GDP on NAICS and SIC basis --------------------------
sgdp.a  # US has real but not nominal
sgdp_sic.a
comment(sgdp.a)
comment(sgdp_sic.a)

sic2 <- sgdp_sic.a %>%
  group_by(stabbr) %>%
  mutate(r_gdp1997=gdp / gdp[year==1997],
         r_rgdp1997=rgdp / rgdp[year==1997]) %>%
  ungroup()

base <- cross_df(list(year=min(sic2$year):max(sgdp.a$year),
                      stabbr=unique(c(sgdp.a$stabbr, sgdp_sic.a$stabbr))))

spliced <- base %>%
  left_join(sic2 %>% rename(gdp_sic=gdp, rgdp_sic=rgdp),
            by=c("stabbr", "year")) %>%
  left_join(sgdp.a %>% rename(gdp_naics=gdp, rgdp_naics=rgdp),
            by=c("stabbr", "year")) %>%
  group_by(stabbr) %>%
  mutate(gdp=ifelse(is.na(gdp_naics), r_gdp1997 * gdp_naics[year==1997], gdp_naics),
         rgdp=ifelse(is.na(rgdp_naics), r_rgdp1997 * rgdp_naics[year==1997], rgdp_naics))

st <- "CA"
spliced %>%
  filter(stabbr==st) %>%
  select(year, stabbr, gdp_sic, r_gdp1997, gdp_naics, gdp) %>%
  mutate(check=gdp_sic / gdp)

spliced %>%
  filter(stabbr==st) %>%
  select(year, stabbr, rgdp_sic, r_rgdp1997, rgdp_naics, rgdp) %>%
  mutate(check=rgdp_sic / rgdp)

sgdp_spliced.a <- spliced %>%
  select(year, stabbr, gdp, rgdp) %>%
  pivot_longer(cols=c(gdp, rgdp)) %>%
  filter(!is.na(value)) %>%
  arrange(name, stabbr, year) %>%
  ungroup

sgdp_spliced.a %>%
  filter(stabbr=="NY", name=="rgdp") %>%
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point()

comment(sgdp_spliced.a) <- paste0("State nominal and real GDP, spliced NAICs and SIC basis, annual, downloaded ", downloaddate)
comment(sgdp_spliced.a)
usethis::use_data(sgdp_spliced.a, overwrite=TRUE)


# Get state quarterly gdp data on NAICS basis -----------------------------
# GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit 2
# GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit 9
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

get_gdpq2 <- function(fn, vname){
  # GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit 2
  # GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit 9
  # fn <- "SQGDP2__ALL_AREAS_2005_2019.csv"
  fullpath <- paste0(bdir, "SQGDP.zip")
  df <- read_delim(unz(fullpath, fn),
                   delim=",",
                   escape_double = FALSE,
                   col_types = cols(GeoFIPS=col_character(),
                                    GeoName=col_character(),
                                    Region=col_double(),
                                    TableName=col_character(),
                                    LineCode=col_integer(),
                                    Unit=col_character(),
                                    IndustryClassification=col_character(),
                                    Description=col_character(),
                                    .default= col_double()))

  df2 <- df %>%
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"),
           GeoName=str_remove(GeoName, "[*]+"),
           stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])

  df3 <- df2 %>% filter(!is.na(stabbr)) %>%
    rename(line=LineCode,
           indclass=IndustryClassification,
           indname=Description) %>%
    select(stabbr, line, indclass, indname, contains(":")) %>%
    gather(yearq, value, -stabbr, -line, -indclass, -indname)

  df4 <- df3 %>%
    mutate(vname=vname,
           date=dq2(yearq),
           value=as.numeric(value)) %>%
    select(vname, stabbr, date, line, indclass, indname, value)

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
(fname_nom <- fnames %>% str_subset("SQGDP2"))
(fname_real <- fnames %>% str_subset("SQGDP9"))
read_csv(unz(zfn, fname_nom), n_max=10) %>% select(c(1:10, ncol(.))) %>% as_tibble
read_csv(unz(zfn, fname_real), n_max=10) %>% select(c(1:10, ncol(.))) %>% as_tibble

# SQGDP11__ALL_AREAS_2005_2018.csv Contributions to percent change in real GDP Percentage points
# SQGDP2__ALL_AREAS_2005_2018.csv Gross domestic product (GDP) by state Millions of current dollars
# SQGDP8__ALL_AREAS_2005_2018.csv Quantity indexes for real GDP by state (2012=100.0)
# SQGDP9__ALL_AREAS_2005_2018.csv Real GDP by state Millions of chained 2012 dollars
# all run from 2005:Q1 (using this date format) to latest
# end peek

ngdpq <- get_gdpq2(fname_nom, "gdp")
ht(ngdpq)

rgdpq <- get_gdpq2(fname_real, "rgdp")
ht(rgdpq)

# save real and nominal gdp, all industries, and then go on and save a slim file
sgdp.q_all <- bind_rows(ngdpq, rgdpq)
comment(sgdp.q_all) <- paste0("State GDP all variables, quarterly, downloaded ", downloaddate)
comment(sgdp.q_all)
count(sgdp.q_all, stabbr)
usethis::use_data(sgdp.q_all, overwrite=TRUE)

# now save slimmed down file
dfslim <- sgdp.q_all %>%
  filter(line==1,) %>%
  select(stabbr, date, vname, value) %>%
  spread(vname, value)
glimpse(dfslim)
ht(dfslim)

sgdp.q <- dfslim
comment(sgdp.q) <- paste0("State nominal and real GDP, quarterly, downloaded ", downloaddate)
comment(sgdp.q)
count(sgdp.q, stabbr)
usethis::use_data(sgdp.q, overwrite=TRUE)

rm(sgdp.q, sgdp.q_all)
load("./data/sgdp.q.rda")


# Get SAINC1 state annual personal income data ----------------------------

fn <- fname_spia

get_spi <- function(fn, vname, lcode=NULL){
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

  if(is.null(lcode)) lcode <- unique(df$LineCode)

  df2 <- df  |>
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"),
           GeoName=str_remove(GeoName, "[*]+") |> str_trim(),
           stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])

  df3 <- df2 %>% filter(!is.na(stabbr)) |>
    rename(line=LineCode,
           spiname=Description) |>
    filter(line %in% lcode) |>
    select(-GeoFIPS, -GeoName, -Region, -TableName, -IndustryClassification, -Unit) |>
    pivot_longer(cols=-c(stabbr, spiname, line), names_to = c("year"))
    # gather(year, value, -stabbr, -spiname, -line)

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
(fnames <- unzip(fn, list=TRUE) %>%
    filter(str_detect(Name, "ALL_AREAS")) %>%
    arrange(Name) %>%
    pull(Name))

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
(fname_spia <- fnames %>% str_subset("SAINC1_"))
(fname_spia_details <- fnames %>% str_subset("SAINC4_"))
spi.a <- get_spi(fname_spia, "spi", lcode=NULL)
# spi.a <- get_spi("SAINC1__ALL_AREAS_1929_2018.csv", "spi", lines=NULL)
ht(spi.a)
count(spi.a, stabbr)

comment(spi.a) <- paste0("State personal income ($m), population (#), pci ($), annual, downloaded ", downloaddate)
usethis::use_data(spi.a, overwrite=TRUE)

load("./data/spi.a.rda")
glimpse(spi.a)
comment(spi.a)
spi.a %>% filter(stabbr=="NY") %>% tail(20)


# Get SAINC4 DETAILED COMPONENTS OF state annual personal income --------
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

spi.a_all <- get_spi(fname_spia_details, "spi")
ht(spi.a_all)
count(spi.a_all, line, spiname)
count(spi.a_all, stabbr)

comment(spi.a_all) <- paste0("State personal income DETAILS, annual, downloaded ", downloaddate)
usethis::use_data(spi.a_all, overwrite=TRUE)

load("./data/spi.a_all.rda")
glimpse(spi.a_all)
comment(spi.a_all)
spi.a_all %>% filter(stabbr=="NY") %>% tail(20)


# Get state quarterly personal income data --------------------------------

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
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"), # remove nonnumeric \,"
           GeoName=str_remove(GeoName, "[*]+") |> str_trim(),
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


download.rzip("SQINC")
downloaddate <- format(Sys.time(), '%Y-%m-%d')
zfn <- paste0(bdir, "SQINC.zip")
fnames_spiq <- unzip(zfn, list=TRUE) %>%
  filter(str_detect(Name, "ALL_AREAS")) %>%
  arrange(Name) %>%
  pull(Name)
fn <- "SQINC1__ALL_AREAS_1948_2022.csv"

(fname_spiq <- fnames_spiq %>% str_subset("SQINC1_"))
(fname_spiq_details <- fnames_spiq %>% str_subset("SQINC4_"))
vname <- "spi"

# save just the state data as sgdp.q - it also has summaries by region

# peek at the files as needed
fn <- "SQINC1__ALL_AREAS_1948_2022.csv"
fn <- "SQINC4__ALL_AREAS_1948_2022.csv"
read_csv(unz(zfn, fn), n_max=10) %>%
  select(c(1:9, ncol(.))) %>%
  as_tibble

# Files guess based on annual files
# SQINC1" Name="Personal Income Summary: Personal Income, Population, Per Capita Personal Income SQINC1__ALL_AREAS_1998_2018.csv
# SQINC35" Name="Personal Current Transfer Receipts SQINC35_1998_2018_ALL_AREAS.csv
# SQINC4" Name="Personal Income and Employment by Major Component SQINC4__ALL_AREAS_1998_2018.csv
# SQINC5N" Name="Personal Income by Major Component and Earnings by NAICS Industry SQINC5N__ALL_AREAS_1998_2018.csv
# SQINC6N" Name="Compensation of Employees by NAICS Industry SQINC6N__ALL_AREAS_1998_2018.csv
# SQINC7N" Name="Wages and Salaries by NAICS Industry SQINC7N__ALL_AREAS_1998_2018.csv
# all run from 1998:Q1 (using this date format) to latest
# end peek

df <- get_spiq(fname_spiq_details, "spi")
ht(df)
count(df, line, description)
count(df, stabbr)

# add vnames
vnames <- read_csv("line, vname
10, spi
11, nonfarmpi
12, farmpi
20, pop
30, pcpi
35, earn_pow
36, c_socins
37, eec_socins
38, erc_socins
42, resadj
45, netearn_por
46, divintrent
47, perstransf
50, wages
60, wagesups
61, erc_pension
62, erc_socins2
70, propinc
71, farmprop_inc
72, nonfarm_propinc")
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
count(spi.q, stabbr)
usethis::use_data(spi.q, overwrite=TRUE)

spiw.q <- spi.q %>%
  select(stabbr, date, vname, value) %>%
  pivot_wider(names_from = vname)
glimpse(spiw.q)
comment(spiw.q) <- paste0("State personal income components and selected other variables, wide, quarterly, downloaded ",
                          downloaddate)
count(spiw.q, stabbr)
usethis::use_data(spiw.q, overwrite=TRUE)


load("./data/spi.q.rda")
glimpse(spi.q)
comment(spi.q)
spi.q %>% filter(stabbr=="NY") %>% tail(20)


# Get state annual personal consumption expenditure data ------------------
# https://apps.bea.gov/regional/zip/SQGDP.zip
# e:\data\BEAData\SAEXP.zip
# con <- unz(fullpath, fn)
# isOpen(con, rw = "")
# isIncomplete(con)
# GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit,1997

get_pce <- function(fn){
  print(fn)
  # GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit,1997
  fullpath <- paste0(bdir, "SAPCE.zip")
  # fn <- "SAPCE1__ALL_AREAS_1997_2020.csv"  # 1, 2, 3, 4
  df <- read_delim(unz(fullpath, fn),
                   delim=",",
                   escape_double = FALSE,
                   # Region is always numeric so previously had not been specified
                   col_types = cols(GeoFIPS=col_character(),
                                    GeoName=col_character(),
                                    Region=col_integer(),
                                    TableName=col_character(),
                                    LineCode=col_integer(),
                                    Unit=col_character(),
                                    IndustryClassification=col_character(),
                                    Description=col_character(),
                                    .default= col_double()))
  # count(df, IndustryClassification)  # IndustryClassification is always ... or NA, so drop

  # this next step may not be necessary for the PCE data, other than getting stabbr
  df2 <- df %>%
    filter(!is.na(LineCode)) %>%
    mutate(GeoFIPS=str_extract(GeoFIPS, "[0-9]+"),
           GeoName=str_remove(GeoName, "[*]+"),
           stabbr=stcodes$stabbr[match(GeoName, stcodes$stname)])
  # count(df2, GeoName, stabbr, Region) # Regions not coded

  # # GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit,1997
  df3 <- df2 %>%
    rename(line=LineCode,
           table=TableName,
           pcename=Description) %>%
    select(-c(GeoFIPS, Region, Unit, IndustryClassification)) %>%
    setNames(str_to_lower(names(.))) %>%
    mutate(pcetype=case_when(table == "SAPCE1" ~ "majorprod",
                             table == "SAPCE2" ~ "majorprodpc",
                             table == "SAPCE3" ~ "detailprod",
                             table == "SAPCE4" ~ "function",
                             TRUE ~ "ERROR"
                             )) %>%
    pivot_longer(cols=-c(table, pcetype, geoname, stabbr, line, pcename), names_to = "year") %>%
    mutate(year=as.integer(year)) %>%
    select(table, pcetype, geoname, stabbr, line, pcename, year, value)

  return(df3)
}


# http://www.bea.gov/regional/zip/PCEbyState.zip
download.rzip("SAPCE")
downloaddate <- format(Sys.time(), '%Y-%m-%d')

vname <- "pce"
fn <- paste0(bdir, "SAPCE.zip")
unzip(fn, list=TRUE) %>% filter(str_detect(Name, "ALL_AREAS")) %>% arrange(desc(Length))
# Name  Length                Date
# 1 SAPCE4__ALL_AREAS_1997_2020.csv 2120531 2021-10-08 06:08:00
# 2 SAPCE3__ALL_AREAS_1997_2020.csv 1809329 2021-10-05 10:16:00
# 3 SAPCE1__ALL_AREAS_1997_2020.csv  414696 2021-10-05 10:16:00
# 4 SAPCE2__ALL_AREAS_1997_2020.csv  294458 2021-10-05 10:16:00

# $ millions, except for per capita
# SAPCE1" Name="Personal consumption expenditures (PCE) by major type of product
# SAPCE2" Name="Per capita personal consumption expenditures (PCE) by major type of product
# SAPCE3" Name="Personal consumption expenditures (PCE) by state by type of product
# SAPCE4" Name="Personal consumption expenditures (PCE) by state by function

# 1: GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit,1997...
# 2: GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit,1997...
# 3: GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit,1997,...
# 4: GeoFIPS,GeoName,Region,TableName,LineCode,IndustryClassification,Description,Unit,1997,...


flist <- c("SAPCE1__ALL_AREAS_1997_2020.csv",
           "SAPCE2__ALL_AREAS_1997_2020.csv",
           "SAPCE3__ALL_AREAS_1997_2020.csv",
           "SAPCE4__ALL_AREAS_1997_2020.csv")
spce.a <- map_dfr(flist, get_pce)
count(spce.a, year)
count(spce.a, table, pcetype)
count(spce.a, geoname, stabbr)
glimpse(spce.a)
ht(spce.a)

comment(spce.a) <- paste0("State personal consumption expenditures, annual, downloaded ", downloaddate)
usethis::use_data(spce.a, overwrite=TRUE)

load("./data/spce.a.rda")
glimpse(spce.a)
comment(spce.a)
spce.a %>% filter(stabbr=="NY") %>% ht(15)


# Review package after documenting, building, checking ----------------------------------------------------------
data(package="BEAData")

