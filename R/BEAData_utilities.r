
#' @import dplyr
NULL # applies everywhere (to all functions)


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
#' library("dplyr")
#' getgdppi()
#' getgdppi("Q")
getgdppi <- function(gfreq="A"){
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
#' library("dplyr")
#' getNIPATable("1.1.1")
#' getNIPATable("3.3")
getNIPATable <- function(gtabnum) {
  BEAData::NIPAvars %>% filter(tabnum==gtabnum) %>%
    arrange(line) %>%
    select(vname, line, vdesc, tabname)
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
#' library("dplyr")
#' getNIPAvarinfo("3.3", 1)
#' getNIPAvarinfo("2.3.2", 4)
getNIPAvarinfo <- function(gtabnum, gline){
  vname <- BEAData::NIPAvars %>%
    filter(tabnum==gtabnum, line==gline) %>%
    select(tabnum, line, vname, vdesc, tabname)
  return(vname)
}


