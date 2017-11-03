

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


#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
ma3 <-  function(x) {
  zoo::rollapply(x, 3, function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")
}

ma <-  function(x, periods) {
  zoo::rollapply(x, periods, function(x) mean(x, na.rm = TRUE), fill = NA, align = "right")
}

#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
srr <- "D:/Dropbox/LucyOrHeatherT/SRR/2 SRR DATA/"
pitfn <- "SRR-PIT-2017_v2.xlsm"

# S1=Based on Census advance data, seasonally adjusted.
# S2=Based on Census preliminary and final data, seasonally adjusted.
# S3=Based on unpublished Census data, seasonally adjusted.
# S4=Based on unpublished Census data, seasonally adjusted by BEA.


#****************************************************************************************************
#                Get ALL BEA econ data ####
#****************************************************************************************************
# get all of the files ----
b.url <- "https://www.bea.gov/national/Release/TXT/FlatFiles.ZIP"
b.file <- tempfile()
system.time(download.file(b.url, b.file, mode="wb")) # 12 secs
unzip(b.file, list=TRUE)

vars <- read_csv(unz(b.file, "SeriesRegister.txt"))
vars <- vars %>%
  rename(series=`%SeriesCode`,
         TableId_LineNo=`TableId:LineNo`)
glimpse(vars)

tabs <- read_csv(unz(b.file, "TablesRegister.txt"))
glimpse(tabs)

vartabs <- vars %>%
  select(-DefaultScale, -SeriesCodeParents) %>%
  separate(TableId_LineNo, sep="\\|", into=paste0("tl", 1:20)) %>%
  gather(tlnum, tl, starts_with("tl")) %>%
  select(-tlnum) %>%
  filter(!is.na(tl)) %>%
  separate(tl, sep="\\:", into=c("table", "line")) %>%
  mutate(line=as.integer(line)) %>%
  arrange(series, table, line) %>%
  left_join(tabs %>% rename(table=TableId))
glimpse(vartabs)

vartabs %>% filter(series=="A001RC")
vartabsfirst <- vartabs %>%
  group_by(series) %>%
  arrange(table, line) %>%
  filter(table==first(table))

vartabs %>%
  filter(table=="T10103") %>%
  arrange(line) %>%
  mutate(TableTitle=str_sub(TableTitle, 1, 30))


da <- read_csv(unz(b.file, "nipadataA.txt"))
da2 <- da %>%
  rename(series=`%SeriesCode`,
         year=Period) %>%
  setNames(str_to_lower(names(.))) %>%
  mutate(freq="A")
glimpse(da2)
ht(da2)
anyDuplicated(da2)
da3 <- da2 %>%
  left_join(vars %>% select(series, SeriesLabel, MetricName, CalculationType))
ht(da3)



dq <- read_csv(unz(b.file, "nipadataQ.txt"))
dq2 <- dq %>%
  rename(series=`%SeriesCode`,
         value=Value) %>%
  mutate(date=as.Date(ymd(paste(str_sub(Period, 1, 4),
                                as.integer(str_sub(Period, -1)) * 3 - 2,
                                "01", sep="-")))) %>%
  select(series, date, value) %>%
  mutate(freq="Q")
glimpse(dq2)
ht(dq2)
anyDuplicated(dq2)
dq3 <- dq2 %>%
  left_join(vars %>% select(series, SeriesLabel, MetricName, CalculationType))
ht(dq3)


dm <- read_csv(unz(b.file, "nipadataM.txt"))
dm2 <- dm %>%
  rename(series=`%SeriesCode`, value=Value) %>%
  mutate(date=as.Date(ymd(paste(str_sub(Period, 1, 4),
                                str_sub(Period, -2, -1),
                                "01", sep="-")))) %>%
  select(-Period)
dm2 %>% filter(series=="A034RC") %>% ht
dm3 <- dm2 %>%
  left_join(vartabsfirst %>% select(series, SeriesLabel, MetricName, CalculationType, table_first=TableTitle, line_first=line))
ht(dm3)

count(dm3, series, SeriesLabel)
dm3 %>% filter(series=="A015RC")



#****************************************************************************************************
#                Get econ data ####
#****************************************************************************************************
# https://www.bea.gov/iTable/iTable.cfm?ReqID=19&step=4&isuri=1&1921=flatfiles
# https://bea.gov/national/Release/TXT/FlatFiles.ZIP


vars <- read_csv("https://bea.gov/national/Release/TXT/SeriesRegister.txt")
vars <- vars %>%
  rename(series=`%SeriesCode`,
         TableId_LineNo=`TableId:LineNo`)
glimpse(vars)

tabs <- read_csv("https://bea.gov/national/Release/TXT/TablesRegister.txt")
glimpse(tabs)

da <- read_csv("https://bea.gov/national/Release/TXT/NipaDataA.txt")
da2 <- da %>%
  rename(series=`%SeriesCode`,
         year=Period) %>%
  setNames(str_to_lower(names(.)))
glimpse(da2)
ht(da2)

system.time(dq <- read_csv("https://bea.gov/national/Release/TXT/NipaDataQ.txt")) # 14 seconds
dq2 <- dq %>%
  rename(series=`%SeriesCode`,
         value=Value) %>%
  mutate(date=as.Date(ymd(paste(str_sub(Period, 1, 4),
                                as.integer(str_sub(Period, -1)) * 3 - 2,
                                "01", sep="-")))) %>%
  select(series, date, value)
glimpse(dq2)
ht(dq2)

da2 %>% filter(series=="A072RC", year>=2005) %>%
  ggplot(aes(year, value)) +
  geom_line()

dq2 %>% filter(series=="A072RC", year(date) >= 2005) %>%
  ggplot(aes(date, value)) +
  geom_line()



dm <- read_csv("https://www.bea.gov/national/Release/TXT/NipaDataM.txt")
glimpse(dm)
ht(dm)
dm2 <- dm %>%
  rename(series=`%SeriesCode`, value=Value) %>%
  mutate(date=as.Date(ymd(paste(str_sub(Period, 1, 4),
                                str_sub(Period, -2, -1),
                                "01", sep="-"))))
dm2 %>% filter(series=="A034RC") %>% ht






b.file <- tempfile("FlatFiles", b.dir, ".zip")

# Underlying details
# https://www.bea.gov/iTable/index_UD.cfm















edf <- readRDS("./data/nipa_m.rds") # created by get_econ_monthly_data_FTA.r
glimpse(edf)
count(edf, TableID, Description)

varinfo <- edf %>% count(TableID, LineNumber, SeriesCode, LineDescription, Description)

edf %>% filter(TableID=="76") %>% count(LineNumber, SeriesCode, LineDescription) # 2.6 Personal income
edf %>% filter(TableID=="2009") %>% count(LineNumber, SeriesCode, LineDescription) # 5U BEA Retail and Food Service
edf %>% filter(TableID=="2017") %>% count(LineNumber, SeriesCode, LineDescription) # Table 2.4.5U. Personal Consumption Expenditures by Type
# A034RC wages and salaries
# T05001 S1  Retail and food services   307

vars <- read_csv("table, series, vname
76, A065RC, pi
76, A034RC, wages
76, A067RC, dpi
76, A071RC, persaving
2009, T05001, retailfoodsvcs
2009, T05002, retail
2009, T05004, autodealer
2009, T05041, restbars
2017, DPCERC, pce
2017, DDURRC, durgoods
2017, DMOTRC, mvandparts
2017, DNDGRC, nondurgoods
2017, DFOFRC, foodoffprem
2017, DNBVRC, nabevoffprem
2017, DGOERC, gasenergy
2017, DPHMRC, medical
2017, DNFRRC, abroad
2017, DELGRC, elecgas
2017, DMVSRC, mvservices
2017, DRCARC, recreation
2017, DGAMRC, gambling
2017, DFSERC, foodsvc
2017, DMSLRC, schoolmeals
2017, DHOTRC, hotelsvc
2017, DTCSRC, telecommsvc
2017, DPCSRC, perscaresvc
2017, DCFSRC, clothingsvc
2017, DHHMRC, hhsvc
                 ") %>%
  filter(!is.na(vname)) %>%
  mutate(table=as.character(table))
vars

econ <- vars %>%
  left_join(edf %>% select(table=TableID, series=SeriesCode, date, value)) %>%
  unique # duplicate gasenergy
glimpse(econ)

taxcons <- econ %>%
  filter(table=="2017" | (table=="76" & vname %in% c("dpi", "persaving"))) %>%
  select(vname, date, value) %>%
  spread(vname, value) %>%
  mutate(taxcons=durgoods +
           nondurgoods - foodoffprem - nabevoffprem - medical - abroad +
           elecgas + mvservices + (recreation - gambling) + (foodsvc - schoolmeals) + hotelsvc + telecommsvc + perscaresvc + clothingsvc + hhsvc,
         gasenergygs=gasenergy + elecgas,
         nonenergy=taxcons - gasenergygs,
         nonenergymv=taxcons - gasenergygs - mvandparts,
         untaxed=pce - taxcons,
         savingsrate=persaving / dpi * 100) %>%
  select(date, taxcons, gasenergygs, nonenergy, nonenergymv, untaxed, savingsrate) %>%
  gather(vname, value, -date) %>%
  mutate(series=vname,
         table="calc") %>%
  select(table, series, vname, date, value)

econ2 <- econ %>%
  bind_rows(taxcons) %>%
  mutate(stabbr="US") %>%
  select(vname, stabbr, date, value)


#****************************************************************************************************
#                Get tax data ####
#****************************************************************************************************

#.. Withholding data ####
wh <- read_excel(paste0(srr, pitfn), sheet="withholding", skip=2, col_types="text")
glimpse(wh)

wh2 <- wh %>%
  mutate(date=as.Date(as.numeric(Withholding), origin = "1899-12-30")) %>%
  filter(!is.na(date)) %>% # keep monthly, drop quarterly
  select(date, one_of(c(state.abb, "DC", "US"))) # we only get income tax states

wh3 <- wh2 %>%
  gather(stabbr, value, -date) %>%
  mutate(value=as.numeric(value), vname="withholding") %>%
  arrange(stabbr, date) %>%
  select(vname, stabbr, date, value)
sum(is.na(wh3$value))
count(wh3, date) %>% ht
count(wh3, stabbr)
glimpse(wh3)


#.. Estimated payments data ####
ep <- read_excel(paste0(srr, pitfn), sheet="estimated", skip=2, col_types="text")
glimpse(ep)

ep2 <- ep %>%
  mutate(date=as.Date(as.numeric(Estimated), origin = "1899-12-30")) %>%
  filter(!is.na(date)) %>% # keep monthly, drop quarterly
  select(date, one_of(c(state.abb, "DC", "US"))) # we only get income tax states

ep3 <- ep2 %>%
  gather(stabbr, value, -date) %>%
  mutate(value=as.numeric(value), vname="estimated") %>%
  arrange(stabbr, date) %>%
  select(vname, stabbr, date, value)
glimpse(ep3)


#.. Final returns data ####
fp <- read_excel(paste0(srr, pitfn), sheet="FinalPayments", skip=2, col_types="text")
glimpse(fp)

fp2 <- fp %>%
  mutate(date=as.Date(as.numeric(`Final Payments`), origin = "1899-12-30")) %>%
  filter(!is.na(date)) %>% # keep monthly, drop quarterly
  select(date, one_of(c(state.abb, "DC", "US"))) # we only get income tax states

fp3 <- fp2 %>%
  gather(stabbr, value, -date) %>%
  mutate(value=as.numeric(value), vname="finals") %>%
  arrange(stabbr, date) %>%
  select(vname, stabbr, date, value)
glimpse(fp3)


#.. Sales tax data ####
srrfn <- "SRR-2017_v2.xlsm"
sut <- read_excel(paste0(srr, srrfn), sheet="RIG-M-sales", skip=0, col_types="text")
glimpse(sut)

sut2 <- sut %>%
  mutate(cdate=paste(str_sub(Sales, 1, 4), str_sub(Sales, 6, 7), 1, sep="-"),
         date=as.Date(cdate)) %>%
  filter(!is.na(date)) %>% # keep monthly, drop quarterly
  select(date, one_of(c(state.abb, "DC", "US")))

sut3 <- sut2 %>%
  gather(stabbr, value, -date) %>%
  mutate(value=as.numeric(value), vname="sales") %>%
  arrange(stabbr, date) %>%
  select(vname, stabbr, date, value)
glimpse(sut3)

#.. Combine tax data ####
tax <- bind_rows(wh3, ep3, fp3, sut3)
glimpse(tax)
count(tax, vname)



#****************************************************************************************************
#                Combine econ and tax data ####
#****************************************************************************************************
count(econ, series, vname)


etax <- bind_rows(econ2, tax) %>%
  filter(!is.na(value)) %>%
  arrange(vname, stabbr, date) %>%
  group_by(vname, stabbr) %>%
  mutate(pchya=value / value[match(date - months(12), date)] * 100 - 100,
         pchya_ma3=ma3(pchya),
         valma3=ma3(value),
         ma3_pchya=valma3 / valma3[match(date - months(12), date)] * 100 - 100) %>%
  ungroup
count(etax, vname)



#****************************************************************************************************
#                WH equation ####
#****************************************************************************************************
glimpse(etax)
whd1 <- etax %>%
  filter(stabbr=="US", vname %in% c("wages", "withholding")) %>%
  select(vname, date, pchya) %>%
  spread(vname, pchya) %>%
  filter(!is.na(wages), !is.na(withholding))

whm1.1 <- lm(withholding ~ wages, data=whd1)
summary(whm1.1)

whm1.2 <- lm(withholding ~ wages - 1, data=whd1)
summary(whm1.2)

whd2 <- etax %>%
  filter(stabbr=="US", vname %in% c("wages", "withholding"), month(date) %in% c(1, 4, 7, 10)) %>%
  select(vname, date, ma3_pchya) %>%
  spread(vname, ma3_pchya) %>%
  filter(!is.na(wages), !is.na(withholding))

whm2.1 <- lm(withholding ~ wages, data=whd2)
summary(whm2.1)

whm2.2 <- lm(withholding ~ wages - 1, data=whd2)
summary(whm2.2)

whd3 <- etax %>%
  filter(stabbr=="US", vname %in% c("wages", "withholding"), month(date) %in% c(1, 4, 7, 10)) %>%
  select(vname, date, value) %>%
  mutate(value=log(value)) %>%
  spread(vname, value) %>%
  filter(!is.na(wages), !is.na(withholding))

whm3.1 <- lm(withholding ~ wages, data=whd2)
summary(whm3.1)

whm3.2 <- lm(withholding ~ wages - 1, data=whd2)
summary(whm3.2)


#****************************************************************************************************
#                Analyze ####
#****************************************************************************************************
glimpse(etax)
count(etax, stabbr)

#..Sales tax ####
stvars <- c("dpi", "untaxed", "pce", "taxcons", "gasenergygs", "nonenergymv", "mvandparts", "sales")

stax <- etax %>% filter(vname %in% stvars)
stax %>%
  filter(year(date)>=2015, stabbr=="US") %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_line() +
  geom_point()

stax %>% filter(year(date)>=2015, stabbr=="US", vname=="sales")

#.. Sales tax median, income, and consumption ----
pvars <- c("dpi", "untaxed", "taxcons", "sales")
pdata <- stax %>%
  filter(year(date)>=2015, vname %in% pvars) %>%
  mutate(usnotus=ifelse(stabbr=="US", "us", "notus")) %>%
  group_by(vname, usnotus, date) %>%
  summarise(ma3_pchya=median(ma3_pchya, na.rm=TRUE),
            pchya_ma3=median(pchya_ma3, na.rm=TRUE)) %>%
  ungroup %>%
  filter((usnotus=="us" & vname!="sales") | (usnotus=="notus")) %>%
  mutate(plotval=ifelse(vname=="sales", pchya_ma3, ma3_pchya))
glimpse(pdata)


p <- pdata %>%
  filter(year(date)>=2015) %>%
  ggplot(aes(date, plotval, colour=vname)) +
  geom_line() +
  geom_point() +
  ggtitle("pchya_ma3 for sales")
p


dbreaks <- seq.Date(as.Date("2015-01-01"), as.Date("2018-01-01"), "3 months")

p2 <- stax %>%
  filter(year(date)>=2016, vname %in% pvars, stabbr=="US") %>%
  mutate(vname=factor(vname,
                      levels=c("dpi", "sales", "taxcons", "untaxed"),
                      labels=c("Disposable personal income", "Sales tax", "Taxable consumption", "Non-taxable consumption"))) %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  scale_x_date(name=NULL, breaks=dbreaks, date_labels="%Y-%b") +
  scale_y_continuous(name="Percent change", limits=c(0, 6), breaks=0:6) +
  ggtitle("Income, consumption, and sales tax (sum of states)",
          subtitle="Year-over-year % change of 3-month moving average") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1.8), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(1.6), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  guides(colour=guide_legend(title=NULL)) +
  labs(caption="Source: Rockefeller Institute and Bureau of Economic Analysis\nNote: Taxable consumption reflects judgment about items commonly taxed by states.")
p2

ggsave(plot=p2, filename="./results/sutecon.png", width=10, height=6)


#.. Consumption components ----
pvars <- c("taxcons", "gasenergygs", "mvandparts", "nonenergymv")
dbreaks <- seq.Date(as.Date("2015-01-01"), as.Date("2018-01-01"), "3 months")

p2 <- stax %>%
  filter(year(date)>=2016, vname %in% pvars, stabbr=="US") %>%
  mutate(vname=factor(vname,
                      levels=c("taxcons", "gasenergygs", "mvandparts", "nonenergymv"),
                      labels=c("Taxable consumption", "Gas and energy", "Motor vehicles & parts", "Everything else\n(Major portion of base)"))) %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  geom_hline(yintercept = 0) +
  scale_x_date(name=NULL, breaks=dbreaks, date_labels="%Y-%b") +
  scale_y_continuous(name="Percent change", breaks=-20:20) + # , limits=c(0, 7)
  ggtitle("Taxable consumption components",
          subtitle="Year-over-year % change of 3-month moving average") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1.8), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(1.6), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  guides(colour=guide_legend(title=NULL)) +
  labs(caption="Source: Rockefeller Institute and Bureau of Economic Analysis\nNote: Taxable consumption reflects judgment about items commonly taxed by states.")
p2

ggsave(plot=p2, filename="./results/txblcons.png", width=10, height=6)


#..Gas and energy goods and services ----
basedate <- "2017-07-01"
lastval <- stax$valma3[stax$date==basedate & stax$vname=="gasenergygs"] / 1000
df <- data.frame(x1 = month(basedate), x2 = 12, y1 = lastval, y2 = lastval)
df

p <- stax %>%
  filter(vname=="gasenergygs") %>%
  mutate(year=year(date) %>% as.factor,
         mnum=month(date)) %>%
  filter(year(date)>=2016) %>%
  ggplot(aes(mnum, valma3 / 1000, colour=year)) +
  geom_point(size=1.5) +
  geom_line(size=1.5) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), colour = "black", linetype="dashed", data = df) +
  scale_x_continuous(name=NULL, breaks=1:12, labels=month.abb) +
  scale_y_continuous(name="$ billions", labels=scales::dollar) +
  ggtitle("Consumption of gas and energy goods and services",
          subtitle="Seasonally adjusted at annual rate, 3-month moving average") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(.8), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  guides(colour=guide_legend(title=NULL)) +
  labs(caption="Source: Rockefeller Institute and Bureau of Economic Analysis")
p
ggsave(plot=p, filename="./results/gasenergygs.png", width=6, height=6)


#..Motor vehicles and parts ----
basedate <- "2017-07-01"
lastval <- stax$valma3[stax$date==basedate & stax$vname=="mvandparts"] / 1000
df <- data.frame(x1 = month(basedate), x2 = 12, y1 = lastval, y2 = lastval)
df

p <- stax %>%
  filter(vname=="mvandparts") %>%
  mutate(year=year(date) %>% as.factor,
         mnum=month(date)) %>%
  filter(year(date)>=2016) %>%
  ggplot(aes(mnum, valma3 / 1000, colour=year)) +
  geom_point(size=1.5) +
  geom_line(size=1.5) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), colour = "black", linetype="dashed", data = df) +
  scale_x_continuous(name=NULL, breaks=1:12, labels=month.abb) +
  scale_y_continuous(name="$ billions", labels=scales::dollar) +
  ggtitle("Consumption of motor vehicles and parts",
          subtitle="Seasonally adjusted at annual rate, 3-month moving average") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(.8), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  guides(colour=guide_legend(title=NULL)) +
  labs(caption="Source: Rockefeller Institute and Bureau of Economic Analysis")
p
ggsave(plot=p, filename="./results/mvandparts.png", width=6, height=6)


#..Withholding and wages ----
pvars <- c("wages", "withholding")
dbreaks <- seq.Date(as.Date("2015-01-01"), as.Date("2018-01-01"), "3 months")

p2 <- etax %>%
  filter(year(date)>=2016, vname %in% pvars, stabbr=="US") %>%
  mutate(vname=factor(vname,
                      levels=c("wages", "withholding"),
                      labels=c("Wages", "Withholding"))) %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  geom_hline(yintercept = 0) +
  scale_x_date(name=NULL, breaks=dbreaks, date_labels="%Y-%b") +
  scale_y_continuous(name="Percent change", breaks=-20:20) + # , limits=c(0, 7)
  ggtitle("Wages and withholding",
          subtitle="Year-over-year % change of 3-month moving average") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1.8), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(1.6), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  guides(colour=guide_legend(title=NULL)) +
  labs(caption="Source: Rockefeller Institute and Bureau of Economic Analysis\nNote: Taxable consumption reflects judgment about items commonly taxed by states.")
p2

ggsave(plot=p2, filename="./results/wageswh.png", width=10, height=6)


#..Withholding and wages, median ----
pvars <- c("wages", "withholding")
dbreaks <- seq.Date(as.Date("2015-01-01"), as.Date("2018-01-01"), "3 months")

p2 <- etax %>%
  filter(year(date)>=2016, (vname=="wages" & stabbr=="US") | (vname=="withholding" & stabbr!="US")) %>%
  group_by(vname, date) %>%
  summarise(ma3_pchya=median(ma3_pchya, na.rm=TRUE)) %>%
  ungroup %>%
  mutate(vname=factor(vname,
                      levels=c("wages", "withholding"),
                      labels=c("Wages", "Withholding"))) %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  scale_x_date(name=NULL, breaks=dbreaks, date_labels="%Y-%b") +
  scale_y_continuous(name="Percent change", breaks=-20:20) + # , limits=c(0, 7)
  ggtitle("U.S. wages and withholding (median of states)",
          subtitle="Year-over-year % change of 3-month moving average") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1.8), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(1.6), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  guides(colour=guide_legend(title=NULL)) +
  labs(caption="Source: Rockefeller Institute and Bureau of Economic Analysis\nNote: Taxable consumption reflects judgment about items commonly taxed by states.")
p2

ggsave(plot=p2, filename="./results/wageswhmdn.png", width=10, height=6)

#..Withholding pchya q4q1 multiple years ----
wh2 <- etax %>%
  filter(vname %in% c("wages", "withholding"), stabbr=="US") %>%
  mutate(year=year(date),
         qtr=quarter(date)) %>%
  filter(qtr %in% c(1, 4)) %>%
  group_by(vname, year, qtr) %>%
  summarise(value=mean(value, na.rm=TRUE)) %>%
  group_by(vname, qtr) %>%
  arrange(year) %>%
  mutate(pchya=value / value[match(year - 1, year)] * 100 - 100)

wh2 <- etax %>%
  filter(vname %in% c("withholding"), stabbr!="US") %>%
  mutate(year=year(date),
         qtr=quarter(date)) %>%
  filter(qtr %in% c(1, 4)) %>%
  group_by(vname, stabbr, year, qtr) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  arrange(vname, stabbr, qtr, year) %>%
  group_by(vname, stabbr, qtr) %>%
  filter(year>2007) %>%
  mutate(pchya=value / value[match(year - 1, year)] * 100 - 100) %>%
  group_by(vname, year, qtr) %>%
  summarise(pchya=median(pchya, na.rm=TRUE)) %>%
  ungroup
wh2



qshares <- etax %>%
  filter(vname %in% c("withholding"), stabbr!="US") %>%
  mutate(year=year(date),
         qtr=paste0("q", quarter(date))) %>%
  filter(qtr %in% c("q1", "q4")) %>%
  group_by(stabbr, year, qtr) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  mutate(period=ifelse(qtr=="q4",
                       paste0(year, "-", year + 1),
                       paste0(year - 1, "-", year))) %>%
  ungroup %>%
  select(-year) %>%
  spread(qtr, value) %>%
  mutate(q1ratio=q1 / q4) %>%
  group_by(period) %>%
  summarise(q1ratio=median(q1ratio, na.rm=TRUE))

wageqshares <- etax %>%
  filter(vname %in% c("wages")) %>%
  mutate(year=year(date),
         qtr=paste0("q", quarter(date))) %>%
  filter(qtr %in% c("q1", "q4")) %>%
  group_by(year, qtr) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  mutate(period=ifelse(qtr=="q4",
                       paste0(year, "-", year + 1),
                       paste0(year - 1, "-", year))) %>%
  ungroup %>%
  select(-year) %>%
  spread(qtr, value) %>%
  mutate(q1ratio=q1 / q4) %>%
  filter(period > "2007-08")



stqshares <- etax %>%
  filter(vname %in% c("withholding"), stabbr!="US") %>%
  mutate(year=year(date),
         qtr=paste0("q", quarter(date))) %>%
  filter(qtr %in% c("q1", "q4")) %>%
  group_by(stabbr, year, qtr) %>%
  summarise(value=sum(value, na.rm=TRUE)) %>%
  mutate(period=ifelse(qtr=="q4",
                       paste0(year, "-", year + 1),
                       paste0(year - 1, "-", year))) %>%
  ungroup %>%
  select(-year) %>%
  spread(qtr, value) %>%
  mutate(q1share=q1 / (q1 + q4) * 100) %>%
  filter(period>"2007-08")

stqshares %>% filter(stabbr=="NY")


#****************************************************************************************************
#                Get qtax ####
#****************************************************************************************************
tot <- read_excel(paste0(srr, srrfn), sheet="Q-Total", col_types ="text")
glimpse(tot)
which(names(tot)=="2017q2")
tot2 <- tot[, 1:43] %>%
  rename(region=X__1, stabbr=X__2, stname=Total) %>%
  mutate(stabbr=ifelse(stname=="United States", "US", stabbr),
         region=ifelse(stname=="United States", "US", region)) %>%
  gather(period, value, -region, -stabbr, -stname) %>%
  mutate(value=as.numeric(value),
         vname="total",
         date=yq(period)) %>%
  select(stabbr, region, date, vname, value)
glimpse(tot2)
count(tot2, region, stabbr)
count(tot2, date)

getqtax <- function(sheetname, varname){
  df <- read_excel(paste0(srr, srrfn), sheet=sheetname, col_types ="text")
  lastcol <- which(names(df)=="2017q2")
  print(lastcol)
  names(df)[1:3] <- c("region", "stabbr", "stname")
  df2 <- df[, 1:lastcol] %>%
    mutate(stabbr=ifelse(stname=="United States", "US", stabbr),
           region=ifelse(stname=="United States", "US", region)) %>%
    filter(stabbr %in% c(state.abb, "DC", "US")) %>%
    gather(period, value, -region, -stabbr, -stname) %>%
    mutate(value=as.numeric(value),
           vname=varname,
           date=yq(period),
           year=year(date),
           qtr=quarter(date)) %>%
    select(stabbr, region, date, year, qtr, vname, value)
  return(df2)
}

tot <- getqtax("Q-Total", "total")
pit <- getqtax("Q-PIT", "pit")
sales <- getqtax("Q-Sales", "sales")
cit <- getqtax("Q-CIT", "cit")

qtax <- bind_rows(tot, pit, sales, cit)
glimpse(qtax)
count(qtax, region, stabbr)
count(qtax, date)
count(qtax, vname)
count(qtax, qtr)

qtax2 <- qtax %>%
  group_by(vname, stabbr, qtr) %>%
  arrange(year) %>%
  mutate(pchya=value / value[match(year - 1, year)] * 100 - 100,
         pchya=ifelse(is.infinite(pchya), NA_real_, pchya)) %>%
  ungroup
glimpse(qtax2)
precis(qtax2)


#****************************************************************************************************
#                Line graphs qtax ####
#****************************************************************************************************

dbreaks <- seq.Date(as.Date("2010-01-01"), as.Date("2018-01-01"), "3 months")
dlabs <- paste0(year(dbreaks), "q", quarter(dbreaks))

p1 <- qtax2 %>%
  filter(stabbr=="US", date >= "2015-01-01", vname!="cit") %>%
  ggplot(aes(date, pchya, colour=vname)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="% change vs. year ago", breaks=seq(-40, 40, 2)) +
  scale_x_date(name=NULL,
               breaks=dbreaks,
               labels=dlabs) +
  ggtitle("State tax revenue vs. year earlier",
          subtitle="Year-over-year % change, sum of states, through 2017q2 (preliminary)") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1.8), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(1.6), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(face="bold")) +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold")) +
  theme(axis.text.y=element_text(face="bold")) +
  guides(colour=guide_legend(title=NULL))


p2 <- qtax2 %>%
  filter(stabbr!="US", date >= "2015-01-01", vname!="cit") %>%
  group_by(date, vname) %>%
  summarise(pchya=median(pchya, na.rm=TRUE)) %>%
  ggplot(aes(date, pchya, colour=vname)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="% change vs. year ago", breaks=seq(-40, 40, 1)) +
  scale_x_date(name=NULL,
               breaks=dbreaks,
               labels=dlabs) +
  ggtitle("State tax revenue vs. year earlier",
          subtitle="Year-over-year % change in median state, through 2017q2 (preliminary)") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1.8), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(1.6), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(face="bold")) +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold")) +
  theme(axis.text.y=element_text(face="bold")) +
  guides(colour=guide_legend(title=NULL))
p2
ggsave(plot=p2, filename="./results/taxmdn.png", width=10, height=6)

p1
p2


#****************************************************************************************************
#                Compare average and median ####
#****************************************************************************************************
startdate <- "2016-01-01"

pavg <- qtax2 %>%
  filter(stabbr=="US", date >= startdate, vname!="cit") %>%
  ggplot(aes(date, pchya, colour=vname)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="% change vs. year ago", breaks=seq(-40, 40, 1)) +
  scale_x_date(name=NULL,
               breaks=dbreaks,
               labels=dlabs) +
  ggtitle("State tax revenue vs. year earlier",
          subtitle="Year-over-year % change, sum of states, through 2017q2 (preliminary)") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1.8), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(1.6), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(face="bold")) +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold")) +
  theme(axis.text.y=element_text(face="bold")) +
  guides(colour=guide_legend(title=NULL))


pmdn <- qtax2 %>%
  filter(stabbr!="US", date >= startdate, vname!="cit") %>%
  group_by(date, vname) %>%
  summarise(pchya=median(pchya, na.rm=TRUE)) %>%
  ggplot(aes(date, pchya, colour=vname)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="% change vs. year ago", breaks=seq(-40, 40, 1)) +
  scale_x_date(name=NULL,
               breaks=dbreaks,
               labels=dlabs) +
  ggtitle("State tax revenue vs. year earlier",
          subtitle="Year-over-year % change in median state, through 2017q2 (preliminary)") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(1.8), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(1.6), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=12)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(face="bold")) +
  theme(axis.text.x=element_text(angle=45, hjust=1, face="bold")) +
  theme(axis.text.y=element_text(face="bold")) +
  guides(colour=guide_legend(title=NULL))
ggsave(plot=pmdn, filename="./results/taxmdn.png", width=10, height=6)


pavg
pmdn



library("grid")
library("gridExtra") # to create objects to place on graphs
ml <- marrangeGrob(list(pavg, pmdn), nrow=2, ncol=1)
ml


pavgmdn <- qtax2 %>%
  filter(date >= "2015-01-01", vname!="cit") %>%
  mutate(totmdn=ifelse(stabbr=="US", "Sum of states", "Median state")) %>%
  group_by(date, vname, totmdn) %>%
  summarise(pchya=median(pchya, na.rm=TRUE)) %>%
  ggplot(aes(date, pchya, colour=totmdn)) +
  geom_line(size=1.1) +
  geom_point(size=1.1) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values=c("red", "blue")) +
  scale_y_continuous(name="% change vs. year ago", breaks=seq(-40, 40, 1)) +
  scale_x_date(name=NULL,
               breaks=dbreaks,
               labels=dlabs) +
  ggtitle("State tax revenue vs. year earlier, through 2017q2 (preliminary)",
          subtitle="Sum of states compared to median state") +
  theme_bw() +
  theme(plot.title=element_text(size=rel(.8), face="bold")) +
  theme(plot.subtitle=element_text(size=rel(.6), face="bold")) +
  theme(plot.caption = element_text(hjust=0, size=10)) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(face="bold")) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  # theme(axis.text.y=element_text(face="bold")) +
  theme(strip.text = element_text(face="bold")) +
  guides(colour=guide_legend(title=NULL)) +
  facet_wrap(~vname, nrow=3)

ggsave(plot=pavgmdn, filename="./results/avgmdn.png", width=5, height=6)


qtax2 %>% filter(date=="2017-04-01", vname=="sales") %>%
  arrange(desc(pchya))






#****************************************************************************************************
#                Facet map sales tax ####
#****************************************************************************************************
# supress warnings that we'll get from windowsFonts
# Lucida Sans Typewriter is a monspace font, and I want a good monospace font for maps
windowsFonts(mono2=windowsFont("TT Lucida Sans Typewriter")) # default mono is Courier - associate mono2 with my preferred monospace

map48 <- map_data("state")
map48$stabbr <- as.character(factor(map48$region, levels=tolower(stcodes$stname), labels=stcodes$stabbr))
# ht(map48)

# construct data frame with state centers and stabbr to annotate the map
# with a dummy group variable on the file so ggplot does not get confused, since we are using group in these maps
stabbrmaplabels <- data.frame(state.center, stabbr=state.abb, group=1) %>%
  filter(!stabbr %in% c("AK", "HI"))


pdata <- qtax2 %>%
  filter(stabbr!="US", vname=="sales", date >= as.Date("2014-01-01")) %>%
  mutate(datelab=paste0(year, "q", qtr)) %>%
  select(stabbr, date, value=pchya)

gtitle <- paste0("Percent change in sales tax vs. year ago")
# gtitle <- paste0("Percent change in inflation-adjusted taxes vs. start of recession")

# cut(pdata$value, 4)
cutpts <- c(min(pdata$value, na.rm=TRUE), -5, 0, 5, max(pdata$value, na.rm=TRUE)) # for long periods
# cutpts <- c(min(pdata$value, na.rm=TRUE), -12, -8, -4, 0, 4, 8, 12, max(pdata$value, na.rm=TRUE)) # for long periods
# cutpts <- c(min(pdata$value, na.rm=TRUE), -5, 0, 5, 10, max(pdata$value, na.rm=TRUE)) # for long periods
# cutpts <- c(min(pdata$value, na.rm=TRUE), 0, 2, 4, 6, max(pdata$value, na.rm=TRUE)) # for long periods
# cutpts <- c(min(pdata$value, na.rm=TRUE),-4, 0, 4, 8, 12, max(pdata$value, na.rm=TRUE))
pdata$pgroup <- cut(pdata$value, cutpts, include.lowest=TRUE)
# pdata %>% select(stabbr, value, pgroup) %>% rename(pct.change=value) %>% arrange(-pct.change) %>% kable(digits=1)
count(pdata, pgroup)
# brks<-cut(mapdata$value,c(min(mapdata$value,na.rm=TRUE),4,8,0,2,8,max(mapdata$value,na.rm=TRUE)),include.lowest=TRUE)
# brks<-cut_number(mapdata$pdiffrsum4, 5) # use this for equal intervals
# brks <- cut(mapdata$pchya, c1, include.lowest=TRUE)
# cutpts <- c(min(mapdata$value, na.rm=TRUE),-100, -8, -4, 0, 4, 8, max(mapdata$value, na.rm=TRUE))
# brks <- cut(mapdata$value, cutpts, include.lowest=TRUE)

# cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
djbpalette <- c("#f03b20", "#deebf7", "#9ecae1", "#3182bd")
red2blue3 <- c("#de2d26", "#fee0d2", "#deebf7", "#9ecae1", "#3182bd")
# based on http://colorbrewer2.org/
blue4 <- c(rgb(239,243,255, max=255), rgb(189,215,231, max=255), rgb(107,174,214, max=255), rgb(33,113,181, max=255))
red1blue4 <- c("#fee0d2", blue4)
red2blue2 <- c("#de2d26", "#fee0d2", "#EFF3FF", "#2171B5")
djbpalette <- red2blue3

areColors(djbpalette)

# To use for fills, add scale_fill_manual(values=cbPalette)

mapdata <- merge(pdata, map48, by="stabbr")
mapdata <- arrange(mapdata, order) # keep data sorted by polygon order

fam <- "mono2" # font family to use for map annotation: serif mono sans

p <- ggplot(mapdata, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=pgroup)) + # discrete groups according to break groups determined above
  geom_path(colour="gray", linetype=2) +
  coord_map() +
  scale_fill_manual('% change', values=djbpalette)
#scale_fill_brewer('% change', palette="RdBu") + # RdBu Reds Oranges RdPu YlOrBr OrRd

p <- p + labs(x="Source: Rockefeller analysis of data from U.S. Bureau of the Census") +
  theme(axis.title.x=element_text(size=12, colour="black")) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

p <- p + labs(y="") + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
p <- p + labs(title=gtitle) +
  theme(plot.title=element_text(size=16, face="bold")) # size: use number, or relative: rel(2)

q <- p + facet_wrap(~date, ncol=4)
q
ggsave(plot=q, filename="./results/sutmap.png", width=10, height=6)


#****************************************************************************************************
#                Stuff ####
#****************************************************************************************************
wh4 <- wh3 %>%
  group_by(stabbr) %>%
  mutate(pchya=value / value[match(date - months(12), date)] * 100 - 100,
         pchya_ma3=ma3(pchya)) %>%
  ungroup


sts <- c("CA", "IL", "NY")
wh4 %>%
  filter(stabbr %in% sts, year(date)>=2016) %>%
  ggplot(aes(date, pchya, colour=stabbr)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(-100, 100, 2)) +
  geom_hline(yintercept = 0) +
  ggtitle("Withholding")


sts <- c("CA", "IL", "NY")
wh4 %>%
  filter(stabbr %in% sts, year(date)>=2016) %>%
  ggplot(aes(date, pchya_ma3, colour=stabbr)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(-100, 100, 2)) +
  geom_hline(yintercept = 0) +
  ggtitle("Withholding")


st <- "US"
wh4 %>%
  filter(stabbr==st, year(date)>=2016) %>%
  select(stabbr, date, pchya, pchya_ma3) %>%
  gather(variable, value, pchya, pchya_ma3) %>%
  ggplot(aes(date, value, colour=variable)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(-100, 100, 2)) +
  geom_hline(yintercept = 0) +
  ggtitle(paste0(st, " withholding"))

wh4 %>%
  filter(stabbr!="US", date>="2014-12-01") %>% # year(date)>=2014
  group_by(date) %>%
  summarise_at(vars(pchya, pchya_ma3), funs(median(., na.rm=TRUE))) %>%
  gather(variable, value, pchya, pchya_ma3) %>%
  ggplot(aes(date, value, colour=variable)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(-100, 100, 2)) +
  geom_hline(yintercept = 0) +
  ggtitle("Median of withholding")

wh4 %>%
  filter(stabbr!="US", date>="2013-12-01", month(date) %in% c(12, 1)) %>%
  group_by(date) %>%
  summarise_at(vars(pchya, pchya_ma3), funs(median(., na.rm=TRUE))) %>%
  gather(variable, value, pchya, pchya_ma3) %>%
  ggplot(aes(date, value, colour=variable)) +
  geom_point(size=3) +
  #geom_line() +
  scale_y_continuous(breaks=seq(-100, 100, 2)) +
  geom_hline(yintercept = 0) +
  ggtitle("Median of withholding percentage changes - Decembers and Januaries")


# examine withholding and wages - first, US
whwage <- wh4 %>%
  mutate(variable=ifelse(stabbr=="US", "wh.US", "wh.mdn")) %>%
  group_by(variable, date) %>%
  summarise_at(vars(pchya, pchya_ma3), funs(median(., na.rm=TRUE))) %>%
  ungroup %>%
  bind_rows(wages2 %>% select(-value))

whwage %>%
  filter(year(date)>=2014) %>%
  ggplot(aes(date, pchya_ma3, colour=variable)) +
  geom_point() +
  geom_line()

whwage %>%
  filter(year(date)>=2009, month(date) %in% c(1, 4, 7, 10), variable=="wages")




econ2 %>%
  filter(year(date)>=2016, vname!="retailfoodsvcs") %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_point() +
  geom_line()

econ2 %>%
  filter(year(date)>=2016, vname=="savingsrate") %>%
  ggplot(aes(date, valma3)) +
  geom_point() +
  geom_line()

econ2 %>%
  ungroup %>%
  filter(vname %in% c("dpi", "taxcons", "pce", "untaxed")) %>%
  filter(year(date)>=2016) %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks=0:10, limits=c(0, 6))

econ2 %>%
  filter(vname %in% c("taxcons", "nonenergymv", "mvandparts")) %>%
  filter(year(date)>=2016) %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_point() +
  geom_line()

econ2 %>%
  filter(vname %in% c("taxcons", "gasenergygs", "nonenergymv", "mvandparts")) %>%
  filter(date>="2016-07-01") %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks=seq(-30, 30, 2)) +
  geom_hline(yintercept = 0)

econ2 %>%
  filter(vname %in% c("pce", "taxcons", "gasenergygs", "nonenergymv", "mvandparts"), year(date)==2016) %>%
  group_by(vname) %>%
  summarise(value=sum(value) / 12) %>%
  mutate(share=value / value[vname=="taxcons"] * 100)



econ2 %>%
  ungroup %>%
  filter(vname %in% c("taxcons", "nonenergy")) %>%
  filter(year(date)>=2016) %>%
  ggplot(aes(date, ma3_pchya, colour=vname)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks=seq(-10, 20, 2))

econ2 %>%
  ungroup %>%
  filter(vname %in% c("gasenergy")) %>%
  filter(year(date)>=2015) %>%
  ggplot(aes(date, value, colour=vname)) +
  geom_point() +
  geom_line()

econ2 %>%
  filter(vname %in% c("gasenergy")) %>%
  mutate(year=year(date) %>% as.factor, mnum=month(date), mname=month.abb[mnum]) %>%
  filter(year(date)>=2016) %>%
  ggplot(aes(mnum, value, colour=year)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=1:12, labels=month.abb)
# + scale_y_continuous(breaks=seq(-10, 20, 2))



ht(wages2)
wages2 %>%
  filter(year(date)>=2013) %>%
  select(-value, -variable) %>%
  gather(variable, value, -date) %>%
  ggplot(aes(date, value, colour=variable)) +
  geom_point() +
  geom_line()
# the bea wage data are SA and so moving average does not change things much


econ2 <- econ %>%
  bind_rows(taxcons) %>%
  arrange(vname, series, date) %>%
  group_by(vname, series) %>%
  mutate(pchya=value / value[match(date - months(12), date)] * 100 - 100,
         pchya_ma3=ma3(pchya),
         valma3=ma3(value),
         ma3_pchya=valma3 / valma3[match(date - months(12), date)] * 100 - 100)
count(econ2, vname)
