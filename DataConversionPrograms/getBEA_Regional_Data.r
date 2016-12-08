
# Note that as of now (11/16/2016) I am using git but have not uploaded this to the github site.

# Regional data sources
# http://www.bea.gov/regional/downloadzip.cfm


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
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
  as.Date(ymd(paste(str_sub(qs, 1, 4), as.numeric(str_sub(qs, 6)) * 3 - 2, 1)))
}



#****************************************************************************************************
#                Get state annual gdp data ####
#****************************************************************************************************
# sometimes BEA calls it gross state product but I believe the formal name is state gross domestic product
# so I use gdp

# save just the state data as sgdp.a - it also has summaries by region
# starts in 1997

download.file("http://www.bea.gov/regional/zip/gsp/gsp_naics_all.zip", "./sourceData/gsp_naics_all.zip", mode="wb")
unzip("./sourceData/gsp_naics_all.zip", list=TRUE)
# unzip("./sourceData/qgsp_all.zip", exdir=str_sub(currd, 1, -2))

df <- read_csv(unz("./sourceData/gsp_naics_all.zip", "gsp_naics_all.csv"))
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
devtools::use_data(sgdp.a_all, overwrite=TRUE)

# now save slimmed down file
df5 <- sgdp.a_all %>% filter(ind==1, component %in% c(200, 900)) %>%
  mutate(vname=ifelse(component==200, "gdp",
                      ifelse(component==900, "rgdp", "error"))) %>%
  select(stabbr, year, vname, value) %>%
  spread(vname, value)
glimpse(df5)

sgdp.a <- df5
devtools::use_data(sgdp.a, overwrite=TRUE)

rm(df, df2, df3, df4, df5)
rm(sgdp.a, sgdp.a_all)


load("./data/sgdp.q.rda")
glimpse(sgdp.q)

# load("./data/sgdp.a.rda")
# glimpse(sgdp.a)

# load("./data/sgdp.q_all.rda")
# glimpse(sgdp.q_all)

load("./data/sgdp.a.rda")


#****************************************************************************************************
#                Get state quarterly gdp data ####
#****************************************************************************************************
# save just the state data as sgdp.q - it also has summaries by region
# starts in 2005q1
ugsp <- "http://www.bea.gov/regional/zip/gsp/qgsp_all.zip"
download.file(ugsp, "./sourceData/qgsp_all.zip", mode="wb")
unzip("./sourceData/qgsp_all.zip", list=TRUE)
# unzip("./sourceData/qgsp_all.zip", exdir=str_sub(currd, 1, -2))

df <- read_csv(unz("./sourceData/qgsp_all.zip", "qgsp_all.csv"))
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
devtools::use_data(sgdp.q_all, overwrite=TRUE)

# now save slimmed down file
df5 <- sgdp.q_all %>% filter(ind==1, component %in% c(200, 900)) %>%
  mutate(vname=ifelse(component==200, "gdp",
                      ifelse(component==900, "rgdp", "error"))) %>%
  select(stabbr, date, vname, value) %>%
  spread(vname, value)
glimpse(df5)

sgdp.q <- df5
devtools::use_data(sgdp.q, overwrite=TRUE)

rm(df, df2, df3, df4, df5)
rm(sgdp.q, sgdp.q_all)

load("./data/sgdp.q.rda")




#****************************************************************************************************
#                Get state annual personal income data ####
#****************************************************************************************************
# http://www.bea.gov/regional/zip/spi.zip
# save just the state data as spi.a - it also has summaries by region
# starts in 1997

download.file("http://www.bea.gov/regional/zip/spi.zip", "./sourceData/spi.zip", mode="wb")
unzip("./sourceData/spi.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
# unzip("./sourceData/spi.zip", exdir=str_sub(currd, 1, -2))

df <- read_csv(unz("./sourceData/spi.zip", "SA1_1929_2015.csv"))
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
downloaddate <- format(Sys.time(), '%Y-%m-%d')
comment(spi.a) <- paste0("State personal income ($b) and population (#k), annual, downloaded ", downloaddate)
devtools::use_data(spi.a, overwrite=TRUE)

load("./data/spi.a.rda")
glimpse(spi.a)
comment(spi.a)
spi.a %>% filter(stabbr=="NY") %>% tail(20)




#****************************************************************************************************
#                Get state quarterly personal income data ####
#****************************************************************************************************
# http://www.bea.gov/regional/zip/sqpi.zip
# save just the state data as spi.a - it also has summaries by region
# starts in 1997

download.file("http://www.bea.gov/regional/zip/sqpi.zip", "./sourceData/sqpi.zip", mode="wb")
unzip("./sourceData/sqpi.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
# unzip("./sourceData/sqpi.zip", exdir=str_sub(currd, 1, -2))

df <- read_csv(unz("./sourceData/sqpi.zip", "SQ4_1948_2016_ALL.csv"))
problems(df)
glimpse(df)
count(df, GeoFIPS, GeoName, Region)
count(df, IndustryClassification) # all are ...
count(df, LineCode, Description)

df2 <- df %>% mutate(stabbr=stcodes$stabbr[match(str_replace(GeoName, "\\*", ""), stcodes$stname)])
count(df2, stabbr, GeoFIPS, GeoName, Region)

df3 <- df2 %>% filter(!is.na(stabbr)) %>%
  rename(line=LineCode,
         description=Description) %>%
  select(-GeoFIPS, -GeoName, -Region, -Table, -IndustryClassification) %>%
  gather(yearq, value, -stabbr, -line, -description) %>%
  mutate(value=as.numeric(value)) %>%
  filter(!is.na(value))
glimpse(df3)
count(df3, line, description)
count(df3, year) %>% ht
count(df3, stabbr) # includes DC and US

df4 <- df3 %>%
  mutate(date=dq(yearq)) %>%
  select(-yearq) %>%
  select(stabbr, date, line, description, value)
glimpse(df4)

# save real and nominal gdp, all industries, and then go on and save a slim file
spi.q <- df4
downloaddate <- format(Sys.time(), '%Y-%m-%d')
comment(spi.q) <- paste0("State personal income components and selected other variables, quarterly, downloaded ", downloaddate)
devtools::use_data(spi.q, overwrite=TRUE)

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

download.file("http://www.bea.gov/regional/zip/PCEbyState.zip", "./sourceData/PCEbyState.zip", mode="wb")
unzip("./sourceData/PCEbyState.zip", list=TRUE) %>% arrange(desc(Length)) %>% head(20)
# unzip("./sourceData/PCEbyState.zip", exdir=str_sub(currd, 1, -2))

df <- read_csv(unz("./sourceData/PCEbyState.zip", "PCE_all.csv"))
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

