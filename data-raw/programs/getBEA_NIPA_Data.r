
# change .gitignore IF want raw data uploaded to github - currently excluded
# change .Rbuildignore if want raw data included in package - currently excluded

# https://github.com/us-bea/bea.R

#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************

library("devtools")

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

# use_build_ignore("data-raw", escape = TRUE, pkg = ".")
# use_build_ignore("./R/dataConversion", escape = TRUE, pkg = ".")


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************

currd <- paste0("./data-raw/", "SectionAll_xls/")

curr <- "http://www.bea.gov//national/nipaweb/GetCSV.asp?GetWhat=SS_Data/SectionAll_xls.zip&Section=11"
hist <- "http://www.bea.gov//national/nipaweb/GetCSV.asp?GetWhat=SS_Data/SectionAll_xls_Hist.zip&Section=11"


#****************************************************************************************************
#                Download data ####
#****************************************************************************************************
download.file(curr, "./data-raw/SectionAll_xls.zip", mode="wb")
download.file(hist, "./data-raw/SectionAll_xls_Hist.zip", mode="wb")

downloaddate <- format(Sys.time(), '%Y-%m-%d')

unzip("./data-raw/SectionAll_xls.zip", list=TRUE) # see what is in the archive
unzip("./data-raw/SectionAll_xls.zip", exdir=str_sub(currd, 1, -2)) # now extract the files


#****************************************************************************************************
#                Get annual data with read_excel - CAUTION: use col_names=FALSE  ####
#****************************************************************************************************
fn <- "Section1all_xls.xls"
fnd <- paste0(currd, fn)
xsheets <- excel_sheets(fnd)
asheets <- str_subset(xsheets, "Ann")
qsheets <- str_subset(xsheets, "Qtr")

sheet <- asheets[1]
# df %>% filter(row_number() > start)
# names(df)

getsheet.a <- function(sheet, fnd){
  df <- read_excel(fnd, sheet, col_names=FALSE)
  df2 <- df %>% mutate_all(as.character)
  names(df2)[1:3] <- c("line", "vdesc", "vname")
  start <- which(df2$line=="Line")
  years <- df2[start, -c(1:3)] %>% t %>% as.numeric
  names(df2) <- c(names(df2)[1:3], years)
  tabname <- df2[1, 1] %>% as.character
  df3 <- df2 %>% filter(row_number() > start, !is.na(line), !is.na(vname)) %>%
    mutate(line=as.integer(line), tabname=tabname) %>%
    filter(!is.na(line)) %>%
    gather(year, value, -tabname, -line, -vdesc, -vname) %>%
    mutate(year=as.integer(year),
           value=as.numeric(value)) %>%
    select(tabname, line, vdesc, vname, year, value) %>%
    arrange(line, year)
  return(df3)
}

getfile.a <- function(fnd){
  xsheets <- excel_sheets(fnd)
  asheets <- str_subset(xsheets, "Ann")
  qsheets <- str_subset(xsheets, "Qtr")
  df <- ldply(asheets, getsheet.a, fnd, .progress="text")
  return(df)
}

df1a <- getfile.a(paste0(currd, "Section1all_xls.xls"))
df2a <- getfile.a(paste0(currd, "Section2all_xls.xls"))
df3a <- getfile.a(paste0(currd, "Section3all_xls.xls"))
df4a <- getfile.a(paste0(currd, "Section4all_xls.xls"))
df5a <- getfile.a(paste0(currd, "Section5all_xls.xls"))
df6a <- getfile.a(paste0(currd, "Section6all_xls.xls"))
df7a <- getfile.a(paste0(currd, "Section7all_xls.xls"))

dfa <- bind_rows(df1a, df2a, df3a, df4a, df5a, df6a, df7a) %>% as_tibble # much faster to list tibble than data frame
glimpse(dfa)

# function to get the table number (use for quarterly, too) ####
tabs <- unique(dfa$tabname) # to test the function

tabnum <- function(tabname){
  lhs <- "(.*Table )" # Everthing up to and including "Table "
  rhs <- "(\\. .*)" # Everything including and after ". " (the . is escaped with \\)
  mid <- "(.)" # Everything
  pattern <- paste0(lhs, "|", mid, "|", rhs)
  tabnum <- gsub(pattern, "\\2", tabname) # keep the second part
  return(tabnum)
}
tabdf <- tibble(tabname=tabs, tabnum=tabnum(tabs))

# get table numbers and save
nipa.a <- dfa %>% mutate(tabnum=tabnum(tabname)) %>%
  select(tabnum, everything())
comment(nipa.a) <- paste0("NIPA data all tables, annual, downloaded ", downloaddate)
glimpse(nipa.a)
devtools::use_data(nipa.a, overwrite=TRUE)

nipa.a %>% filter(tabnum=="3.3") %>%
  count(line, vname, vdesc)


# now create a unique file that only has 1 occurrence of each variable
glimpse(nipa.a)
# get the first appearance of each var name
vnames.a <- nipa.a %>% group_by(vname) %>%
  filter(year==max(year)) %>%
  select(-value, -year) %>%
  group_by(vname, tabname) %>%
  arrange(line) %>%
  filter(row_number()==1) %>% # get first line in the table
  group_by(vname) %>%
  arrange(tabname) %>%
  filter(row_number()==1) # get first table

nipa.au <- nipa.a %>%
  group_by(vname, year, value) %>%
  filter(row_number()==1) %>%
  ungroup %>%
  select(vname, year, value) %>%
  mutate(vdesc=vnames.a$vdesc[match(vname, vnames.a$vname)])
glimpse(nipa.au)
comment(nipa.au) <- paste0("NIPA data unique variables, annual, downloaded ", downloaddate)
comment(nipa.au)
devtools::use_data(nipa.au, overwrite=TRUE)


#****************************************************************************************************
#                Get quarterly data with read_excel - CAUTION: use col_names=FALSE ####
#****************************************************************************************************
fn <- "Section1all_xls.xls"
fnd <- paste0(currd, fn)
xsheets <- excel_sheets(fnd)
asheets <- str_subset(xsheets, "Ann")
qsheets <- str_subset(xsheets, "Qtr")

# sheet <- qsheets[1]
# df %>% filter(row_number() > start)
# names(df)

getsheet.q <- function(sheet, fnd){
  df <- read_excel(fnd, sheet, col_names=FALSE)
  df2 <- df %>% mutate_all(as.character)
  names(df2)[1:3] <- c("line", "vdesc", "vname")
  start <- which(df2$line=="Line")
  years <- df2[start, -c(1:3)] %>% t %>% as.numeric
  qtrs <- df2[start + 1, -c(1:3)] %>% t %>% as.numeric
  yq <- paste0(years, "Q", qtrs)
  names(df2) <- c(names(df2)[1:3], yq)
  tabname <- df2[1, 1] %>% as.character
  df3 <- df2 %>% filter(row_number() > start, !is.na(line), !is.na(vname)) %>%
    mutate(line=as.integer(line), tabname=tabname) %>%
    filter(!is.na(line)) %>%
    gather(yq, value, -tabname, -line, -vdesc, -vname) %>%
    mutate(date=as.Date(yq(yq)),
           value=as.numeric(value)) %>%
    select(tabname, line, vdesc, vname, date, value) %>%
    arrange(line, date)
  return(df3)
}

getfile.q <- function(fnd){
  xsheets <- excel_sheets(fnd)
  asheets <- str_subset(xsheets, "Ann")
  qsheets <- str_subset(xsheets, "Qtr")
  df <- ldply(qsheets, getsheet.q, fnd, .progress="text")
  return(df)
}

df1q <- getfile.q(paste0(currd, "Section1all_xls.xls"))
df2q <- getfile.q(paste0(currd, "Section2all_xls.xls"))
df3q <- getfile.q(paste0(currd, "Section3all_xls.xls"))
df4q <- getfile.q(paste0(currd, "Section4all_xls.xls"))
df5q <- getfile.q(paste0(currd, "Section5all_xls.xls"))
df6q <- getfile.q(paste0(currd, "Section6all_xls.xls"))
df7q <- getfile.q(paste0(currd, "Section7all_xls.xls"))

dfq <- bind_rows(df1q, df2q, df3q, df4q, df5q, df6q, df7q) %>% as_tibble
glimpse(dfq) # don't simply list df unless it is a tibble - that is too slow if a dataframe
count(dfq, date)
unique(dfq$date)

nipa.q <- dfq %>% mutate(tabnum=tabnum(tabname)) %>%
  select(tabnum, everything())
glimpse(nipa.q)

nipa.q %>% filter(tabnum=="3.3") %>%
  count(line, vname, vdesc)
comment(nipa.q) <- paste0("NIPA data all tables, quarterly, downloaded ", downloaddate)
comment(nipa.q)
devtools::use_data(nipa.q, overwrite=TRUE)

# now create a unique file that only has 1 occurrence of each variable
glimpse(nipa.q)
# get the first appearance of each var name
vnames.q <- nipa.q %>% group_by(vname) %>%
  filter(date==max(date)) %>%
  select(-value, -date) %>%
  group_by(vname, tabname) %>%
  arrange(line) %>%
  filter(row_number()==1) %>% # get first line in the table
  group_by(vname) %>%
  arrange(tabname) %>%
  filter(row_number()==1) # get first table

nipa.qu <- nipa.q %>%
  group_by(vname, date, value) %>%
  filter(row_number()==1) %>%
  ungroup %>%
  select(vname, date, value) %>%
  mutate(vdesc=vnames.q$vdesc[match(vname, vnames.q$vname)])
comment(nipa.qu) <- paste0("NIPA data unique variables, quarterly, downloaded ", downloaddate)
comment(nipa.qu)
devtools::use_data(nipa.qu, overwrite=TRUE)


