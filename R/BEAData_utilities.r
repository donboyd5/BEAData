
#' @import tidyverse
NULL


#' Get a data frame with the GDP price index, either annual or quarterly.
#'
#' @param gfreq "A" (annual) or "Q" (quarterly). Defaults to "A".
#' @return data frame with:
#'
#'    year (integer), gdppi (double) if freq=="A"
#'
#'    date (date), gdppi (double) if freq=="Q"
#'
#' @export
#' @examples
#' library("tidyverse")
#' getgdppi()
#' getgdppi("Q")
getgdppi <- function(gfreq="A"){
  vname <- freq <- date <- year <- value <- NULL # fool R CMD check
  gdppi <- BEAData::nipa %>%
    filter(vname=="A191RG", freq==gfreq) %>%
    select(date, year, freq, gdppi=value)
  return(gdppi)
}


#' Return a NIPA table descriptive information as a data frame.
#'
#' @param gtabnum BEA table number to get, as character, with no trailing period. For example, "1.1.1"
#' @return data frame sorted by line number, with:
#'
#'    vname   BEA variable (series) name
#'
#'    line    line number
#'
#'    vdesc   variable description
#'
#'    tabname full table name
#'
#' @export
#' @examples
#' library("tidyverse")
#' getNIPATable("1.1.1")
#' getNIPATable("3.3")
getNIPATable <- function(gtabnum) {
  tabnum <- line <- vname <- vdesc <- tabname <- NULL # fool R CMD check
  BEAData::NIPAvars %>%
    filter(tabnum==gtabnum) %>%
    arrange(line) %>%
    select(vname, line, vdesc, tabname)
}


#' Return a NIPA table descriptive information as a data frame, with data for 2 latest years.
#'
#' @param gtabnum BEA table number to get, as character, with no trailing period. For example, "1.1.1"
#' @return data frame sorted by line number, with:
#'
#'    vname   BEA variable (series) name
#'
#'    line    line number
#'
#'    vdesc   variable description
#'
#'    ynum1   value for 2nd-to-latest year
#'
#'    ynum2   value for latest year
#'
#'    tabname full table name
#'
#' @export
#' @examples
#' library("tidyverse")
#' getNIPATableValue("1.1.1")
#' getNIPATableValue("3.3")
getNIPATableValue <- function(gtabnum) {
  tabnum <- line <- vname <- vdesc <- tabname <- freq <- year <- value <- NULL # fool R CMD check
  maxyear <- BEAData::nipa[BEAData::nipa$freq=="A", ]$year %>% max
  df <- getNIPATable(gtabnum)
  df2 <- BEAData::nipa %>%
    filter(freq=="A",
           vname %in% df$vname,
           year %in% (maxyear - 1):maxyear) %>%
    select(vname, year, value) %>%
    mutate(year=paste0("y", year)) %>%
    spread(year, value)
  df3 <- df %>%
    left_join(df2) %>%
    select(vname, line, vdesc, starts_with("y"), tabname)
  return(df3)
}


#' Get a data frame with information on a variable in a specific BEA table and line number.
#'
#' @param gtabnum BEA table number of desired variable, as character, with no trailing period. For example, "1.1.1".
#' @param gline Line number to get, integer.
#' @return A one-row data frame for the variable, with:
#'
#'    tabnum  BEA table number (just the number, as character)
#'
#'    line    line number
#'
#'    vname   BEA variable (series) name
#'
#'    vdesc   variable description
#'
#'    tabname full table name
#'
#' @export
#' @examples
#' library("tidyverse")
#' getNIPAvarinfo("3.3", 1)
#' getNIPAvarinfo("2.3.2", 4)
getNIPAvarinfo <- function(gtabnum, gline){
  tabnum <- line <- vname <- vdesc <- tabname <- NULL # fool R CMD check
  vname <- BEAData::NIPAvars %>%
    filter(tabnum==gtabnum, line==gline) %>%
    select(tabnum, line, vname, vdesc, tabname)
  return(vname)
}


