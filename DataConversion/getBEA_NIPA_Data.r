

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

# for now use xlsx because readxl bombs
library("xlsx")


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
rdat <- "D:/Dropbox/RItems/RData/"

currd <- paste0("./sourceData/", "SectionAll_xls/")

curr <- "http://www.bea.gov//national/nipaweb/GetCSV.asp?GetWhat=SS_Data/SectionAll_xls.zip&Section=11"
hist <- "http://www.bea.gov//national/nipaweb/GetCSV.asp?GetWhat=SS_Data/SectionAll_xls_Hist.zip&Section=11"


#****************************************************************************************************
#                Download data ####
#****************************************************************************************************
download.file(curr, "./sourceData/SectionAll_xls.zip", mode="wb")
download.file(hist, "./sourceData/SectionAll_xls_Hist.zip", mode="wb")

unzip("./sourceData/SectionAll_xls.zip", list=TRUE)
unzip("./sourceData/SectionAll_xls.zip", exdir=str_sub(currd, 1, -2))


#****************************************************************************************************
#                Get annual data with xlsx ####
#****************************************************************************************************
fn <- "Section1all_xls.xls"
fnd <- paste0(currd, fn)
xsheets <- excel_sheets(fnd)
asheets <- str_subset(xsheets, "Ann")
qsheets <- str_subset(xsheets, "Qtr")

sheet <- asheets[1]
df %>% filter(row_number() > start)
names(df)

getsheet <- function(sheet, fnd){
  df <- read.xlsx(fnd, sheet, header=FALSE, colClasses="character")
  df2 <- df %>% mutate_all(as.character)
  names(df2)[1:3] <- c("line", "vdesc", "vname")
  start <- which(df2$line=="Line")
  years <- df2[start, -c(1:3)] %>% t %>% as.numeric
  names(df2) <- c(names(df2)[1:3], years)
  tabname <- df2[1, 1]
  df3 <- df2 %>% filter(row_number() > start, !is.na(line), !is.na(vname)) %>%
    mutate(line=as.integer(as.character(line)), tabname=tabname) %>%
    filter(!is.na(line)) %>%
    gather(year, value, -tabname, -line, -vdesc, -vname) %>%
    mutate(year=as.integer(year),
           value=as.numeric(value)) %>%
    select(tabname, line, vdesc, vname, year, value) %>%
    arrange(line, year)
  return(df3)
}

getfile <- function(fnd){
  xsheets <- excel_sheets(fnd)
  asheets <- str_subset(xsheets, "Ann")
  qsheets <- str_subset(xsheets, "Qtr")
  getsheet(asheets[1], fnd)
  df <- ldply(asheets, getsheet, fnd, .progress="text")
  return(df)
}

df1 <- getfile(paste0(currd, "Section1all_xls.xls"))
df2 <- getfile(paste0(currd, "Section2all_xls.xls"))
df3 <- getfile(paste0(currd, "Section3all_xls.xls"))
df4 <- getfile(paste0(currd, "Section4all_xls.xls"))
df5 <- getfile(paste0(currd, "Section5all_xls.xls"))
df6 <- getfile(paste0(currd, "Section6all_xls.xls"))
df7 <- getfile(paste0(currd, "Section7all_xls.xls"))

df <- bind_rows(df1, df2, df3, df4, df5, df6, df7)
glimpse(df)

# function to get the table number (use for quarterly, too) ####
tabs <- unique(df$tabname) # to test the function

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
nipa.a <- df %>% mutate(tabnum=tabnum(tabname)) %>%
  select(tabnum, everything())
glimpse(nipa.a)
devtools::use_data(nipa.a, overwrite=TRUE)

nipa.a %>% filter(tabnum=="3.3") %>%
  count(line, vname, vdesc)


# now create a unique file that only has 1 occurrence of each variable
glimpse(nipa.a)
# get the first appearance of each var name
vnames <- nipa.a %>% group_by(vname) %>%
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
  mutate(vdesc=vnames$vdesc[match(vname, vnames$vname)])
devtools::use_data(nipa.au, overwrite=TRUE)




#****************************************************************************************************
#                Get quarterly data with xlsx ####
#****************************************************************************************************
fn <- "Section1all_xls.xls"
fnd <- paste0(currd, fn)
xsheets <- excel_sheets(fnd)
asheets <- str_subset(xsheets, "Ann")
qsheets <- str_subset(xsheets, "Qtr")

# sheet <- qsheets[1]
# df %>% filter(row_number() > start)
# names(df)

getsheet <- function(sheet, fnd){
  df <- read.xlsx(fnd, sheet, header=FALSE, colClasses="character")
  df2 <- df %>% mutate_all(as.character)
  names(df2)[1:3] <- c("line", "vdesc", "vname")
  start <- which(df2$line=="Line")
  years <- df2[start, -c(1:3)] %>% t %>% as.numeric
  qtrs <- df2[start + 1, -c(1:3)] %>% t %>% as.numeric
  yq <- paste0(years, "Q", qtrs)
  names(df2) <- c(names(df2)[1:3], yq)
  tabname <- df2[1, 1]
  df3 <- df2 %>% filter(row_number() > start, !is.na(line), !is.na(vname)) %>%
    mutate(line=as.integer(as.character(line)), tabname=tabname) %>%
    filter(!is.na(line)) %>%
    gather(yq, value, -tabname, -line, -vdesc, -vname) %>%
    mutate(date=as.Date(yq(yq)),
           value=as.numeric(value)) %>%
    select(tabname, line, vdesc, vname, date, value) %>%
    arrange(line, date)
  return(df3)
}

getfile <- function(fnd){
  xsheets <- excel_sheets(fnd)
  asheets <- str_subset(xsheets, "Ann")
  qsheets <- str_subset(xsheets, "Qtr")
  getsheet(asheets[1], fnd)
  df <- ldply(qsheets, getsheet, fnd, .progress="text")
  return(df)
}

df1 <- getfile(paste0(currd, "Section1all_xls.xls"))
df2 <- getfile(paste0(currd, "Section2all_xls.xls"))
df3 <- getfile(paste0(currd, "Section3all_xls.xls"))
df4 <- getfile(paste0(currd, "Section4all_xls.xls"))
df5 <- getfile(paste0(currd, "Section5all_xls.xls"))
df6 <- getfile(paste0(currd, "Section6all_xls.xls"))
df7 <- getfile(paste0(currd, "Section7all_xls.xls"))

df <- bind_rows(df1, df2, df3, df4, df5, df6, df7)

count(df, date)
unique(df$date)

nipa.q <- df %>% mutate(tabnum=tabnum(tabname)) %>%
  select(tabnum, everything())
glimpse(nipa.q)

nipa.q %>% filter(tabnum=="3.3") %>%
  count(line, vname, vdesc)

devtools::use_data(nipa.q, overwrite=TRUE)

# now create a unique file that only has 1 occurrence of each variable
glimpse(nipa.q)
# get the first appearance of each var name
vnames <- nipa.q %>% group_by(vname) %>%
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
  mutate(vdesc=vnames$vdesc[match(vname, vnames$vname)])
devtools::use_data(nipa.qu, overwrite=TRUE)





#****************************************************************************************************
#                Other approaches ####
#****************************************************************************************************


glimpse(df2)

f(asheets[2], fnd)


d <- read.xlsx("./Section1all_xls.xls", afiles[2], header=FALSE, colClasses="character")



library("rio")
export(import("./Section1all_xls.xls"), "./Section1all_xls.xlsx")
d <- import("./Section1all_xls.xls", format="xls", which=2)
d <- import("./Section1all_xls.xls", which=xfiles[2])
d <- import("./Section1all_xls.xls", which=3, readxl=FALSE)

read_excel("./Section1all_xls.xlsx")
excel_sheets("./Section1all_xls.xlsx")


xfiles <- excel_sheets(paste0(currd, "Section1all_xls.xls"))
afiles <- str_subset(xfiles, "Ann")
qfiles <- str_subset(xfiles, "Qtr")

excel_sheets("./Section1all_xls.xlsx")
read_excel("./Section1all_xls.xlsx", sheet=2)

read_excel(paste0(currd, "Section1all_xls.xls"))


read_excel("D:/Dropbox/RPrograms PC/Packages/BEAData/data/SectionAll_xls/Section1all_xls.xls")
read_excel("D:/Dropbox/RPrograms PC/Packages/BEAData/data/SectionAll_xls/Section1all_xls2.xls", sheet=2)

read_excel("D:/Dropbox/RPrograms PC/Packages/BEAData/data/SectionAll_xls/Section2all_xls.xls")

read_excel(paste0(currd, "Section1all_xls.xls"), sheet=3)
read_excel(paste0(currd, "Section1all_xls.xls"), sheet=afiles[1])



library(xlsReadWrite)
xls.getshlib()
df = read.xls("myfile.xls", sheet = 1)

library("gdata")
df <- read.xls("./Section1all_xls.xls", sheet=1)

system.time(df <- read.xls("./Section1all_xls.xls", sheet=2))


library("xlsx")
d <- read.xlsx("./Section1all_xls.xls", afiles[2], header=FALSE, colClasses="character")

d <- read.xlsx2("./Section1all_xls.xls", afiles[2], header=FALSE, colClasses="character")
