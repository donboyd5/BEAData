

#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr

library("scales")
library("hms") # hms, for times.
library("stringr") # stringr, for strings.
library("lubridate") # lubridate, for date/times.
library("forcats") # forcats, for factors.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("RColorBrewer") # for custom map colors

library("btools") # library that I created (install from github)
library("bdata")

library("BEAData")
library("apitools")


# CAUTION ----
# update the description file to ensure that these packages are imported
usethis::use_package("tidyverse")
# usethis::use_package("tidyr")
# usethis::use_package("magrittr")
# END CAUTION ----

#****************************************************************************************************
#                Functions ####
#****************************************************************************************************


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************

# S1=Based on Census advance data, seasonally adjusted.
# S2=Based on Census preliminary and final data, seasonally adjusted.
# S3=Based on unpublished Census data, seasonally adjusted.
# S4=Based on unpublished Census data, seasonally adjusted by BEA.


#****************************************************************************************************
#                Explore ####
#****************************************************************************************************
data(package="BEAData")
glimpse(nipa)
count(nipa, freq)

# vname c, year i, value d, vdesc c; add tabnum.first c, tabname.first c, line i
# tabnum is of form "1.1.1"


#****************************************************************************************************
#                Get ALL BEA econ data from flat files ####
#****************************************************************************************************
# get all of the files ----
b.url <- "https://www.bea.gov/national/Release/TXT/FlatFiles.ZIP"
b.file <- tempfile()
system.time(download.file(b.url, b.file, mode="wb")) # 12 secs
unzip(b.file, list=TRUE)

release_date <- unzip(b.file, list=TRUE)$Date %>% as.Date %>% min


vars <- read_csv(unz(b.file, "SeriesRegister.txt"))
vars2 <- vars %>%
  rename(vname=`%SeriesCode`,
         TableId_LineNo=`TableId:LineNo`) %>%
  mutate(vdesc=paste0(SeriesLabel, ", ", MetricName, ", ", CalculationType))
glimpse(vars2)

tabs <- read_csv(unz(b.file, "TablesRegister.txt"))
glimpse(tabs)

NIPAvars <- vars2 %>%
  select(vname, TableId_LineNo, vdesc) %>%
  separate(TableId_LineNo, sep="\\|", into=paste0("tl", 1:20)) %>%
  gather(tlnum, tl, starts_with("tl")) %>%
  select(-tlnum) %>%
  filter(!is.na(tl)) %>%
  separate(tl, sep="\\:", into=c("table", "line")) %>%
  mutate(line=as.integer(line)) %>%
  arrange(vname, table, line) %>%
  left_join(tabs %>% rename(table=TableId, tabname=TableTitle)) %>%
  mutate(tabnum=str_split_fixed(tabname, " ", 3)[, 2],
         tabnum=ifelse(str_sub(tabnum, -1)==".", str_sub(tabnum, 1, -2), tabnum),
         tabnum=ifelse(str_sub(tabname, 1, 5)=="Table", tabnum, NA))
glimpse(NIPAvars)
d <- count(NIPAvars, table, tabnum, tabname)

# getNIPATable <- function(gtabnum) {
#   NIPAvars %>% filter(tabnum==gtabnum) %>%
#     arrange(line) %>%
#     select(vname, line, vdesc, tabname)
# }
getNIPATable("1.1.1")
getNIPATable("3.3")

tmp <- getNIPATable("3.3")
glimpse(tmp)
tmp %>% select(-tabname)

# gtabnum <- "3.3"
# gtabnum <- "2.3.2"

# getNIPAvarinfo <- function(gtabnum, gline){
#   vname <- NIPAvars %>%
#     filter(tabnum==gtabnum, line==gline)
#   return(vname)
# }
getNIPAvarinfo("3.3", 1)
getNIPAvarinfo("2.3.2", 4)

count(NIPAvars, table)
count(NIPAvars, table) %>% filter(str_sub(table, 1, 1)=="T") %>% ht
count(NIPAvars, table, tabnum) %>% filter(str_sub(table, 1, 1)=="T") %>% ht(20)

NIPAvars %>% filter(vname=="A001RC")

NIPAvars %>%
  filter(table=="T10103") %>%
  arrange(line) %>%
  mutate(tabname=str_sub(tabname, 1, 30))

#..Get annual data ----
dfa <- read_csv(unz(b.file, "nipadataA.txt"))
dfa2 <- dfa %>%
  rename(vname=`%SeriesCode`,
         year=Period) %>%
  setNames(str_to_lower(names(.))) %>%
  mutate(freq="A",
         date=as.Date(paste(year, 1, 1, sep="-"))) %>%
  left_join(vars2 %>% select(vname, vdesc)) %>%
  select(vname, date, year, freq, value, vdesc)
glimpse(dfa2)
ht(dfa2)
anyDuplicated(dfa2)

#..Get quarterly data ----
dfq <- read_csv(unz(b.file, "nipadataQ.txt"))
dfq2 <- dfq %>%
  rename(vname=`%SeriesCode`,
         value=Value) %>%
  mutate(date=as.Date(ymd(paste(str_sub(Period, 1, 4),
                                as.integer(str_sub(Period, -1)) * 3 - 2,
                                "01", sep="-"))),
         year=year(date) %>% as.integer,
         freq="Q") %>%
  left_join(vars2 %>% select(vname, vdesc)) %>%
  select(vname, date, year, freq, value, vdesc)
glimpse(dfq2)
ht(dfq2)
anyDuplicated(dfq2)

#..Get monthly data ----
dfm <- read_csv(unz(b.file, "nipadataM.txt"))
dfm2 <- dfm %>%
  rename(vname=`%SeriesCode`,
         value=Value) %>%
  mutate(date=as.Date(ymd(paste(str_sub(Period, 1, 4),
                                str_sub(Period, -2, -1),
                                "01", sep="-"))),
         year=year(date) %>% as.integer,
         freq="M") %>%
  left_join(vars2 %>% select(vname, vdesc)) %>%
  select(vname, date, year, freq, value, vdesc)
dfm2 %>% filter(vname=="A034RC") %>% ht
glimpse(dfm2)
ht(dfm2)
anyDuplicated(dfm2)


#****************************************************************************************************
#                Combine NIPA data and save ####
#****************************************************************************************************
glimpse(dfa2)
glimpse(dfq2)
glimpse(dfm2)

nipa <- bind_rows(dfa2, dfq2, dfm2)
# downloaddate <- format(Sys.time(), '%Y-%m-%d')

comment(nipa) <- paste0("NIPA data all tables, Annual, quarterly, and monthly, released ", release_date)
comment(nipa)
glimpse(nipa)
devtools::use_data(nipa, overwrite=TRUE)

devtools::use_data(NIPAvars, overwrite=TRUE)


#****************************************************************************************************
#                Explore final file ####
#****************************************************************************************************
glimpse(tabs)
tabs$TableTitle[1:10]
getNIPATable("1.1.6")
var <- "A191RX" # real GDP
var <- "A829RX" # real GDP component, state and local govt
nipa %>%
  filter(vname==var) %>%
  filter(year>=2000) %>%
  ggplot(aes(date, value / 1e6, colour=freq)) +
  geom_line() + geom_point() +
  scale_x_date(date_breaks="2 years", date_labels = "%Y") +
  scale_y_continuous(name="$ billions, real") +
  ggtitle(paste0(var, ": Real GDP component, $ billions"))


memory()



