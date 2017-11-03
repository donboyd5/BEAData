

#' Get a data frame with the GDP price index, either annual or quarterly.
#'
#' @param freq "A" (annual) or "Q" (quarterly). Defaults to "A".
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
  requireNamespace("dplyr", quietly = TRUE)
  gdppi <- nipa %>%
    filter(vname=="A191RG", freq==gfreq) %>%
    select(date, year, freq, gdppi=value)
  return(gdppi)
}


#' Get a data frame with the GDP price index, either annual or quarterly.
#'
#' @param gtabnum BEA table number to get, as character, with no trailing period. For example, "1.1.1"
#' @return data frame sorted by line number, with:
#'
#'    vname   BEA variable (series) name
#'    line    line number
#'    vdesc   variable description
#'    tabname full table name
#'
#' @export
#' @examples
#' library("dplyr")
#' getNIPATable("1.1.1")
#' getNIPATable("3.3")
getNIPATable <- function(gtabnum) {
  requireNamespace("dplyr", quietly = TRUE)
  NIPAvars %>% filter(tabnum==gtabnum) %>%
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
#'    line    line number
#'    vname   BEA variable (series) name
#'    vdesc   variable description
#'    tabname full table name
#'
#' @export
#' @examples
#' library("dplyr")
#' getNIPAvarinfo("3.3", 1)
#' getNIPAvarinfo("2.3.2", 4)
getNIPAvarinfo <- function(gtabnum, gline){
  requireNamespace("dplyr", quietly = TRUE)
  vname <- NIPAvars %>%
    filter(tabnum==gtabnum, line==gline) %>%
    select(tabnum, line, vname, vdesc, tabname)
  return(vname)
}


#' Get a list of NIPA.A or NIPA.Q table numbers and descriptions.
#'
#' @param freq "A" (annual) or "Q" (quarterly). Defaults to "A".
#' @return vector with table numbers and descriptions
#' @export
#' @examples
#' library("stringr")
#' listtabs()
#' listtabs("Q")
listtabs <- function(freq="A") {
  requireNamespace("stringr", quietly = TRUE)
  if(str_to_upper(freq)=="A") tabs <- unique(nipa.a$tabname) else
    if(str_to_upper(freq)=="Q") tabs <- unique(nipa.q$tabname) else
      tabs <- "ERROR: freq must be A or Q"
    return(tabs)
}


#' Show values in a NIPA.A or NIPA.Q table, in line order, for the latest year or quarter.
#'
#' @param tabnumc the table number, as character (e.g., "1.1.1" or "1.1.5")
#' @param freq "A" (annual) or "Q" (quarterly). Defaults to "A".
#' @return vector with table numbers and descriptions
#' @export
#' @examples
#' library("dplyr")
#' showtab("1.1.1")
#' showtab("1.1.1", "Q")
showtab <- function(tabnumc, freq="A"){
  requireNamespace("dplyr", quietly = TRUE)
  if(freq=="A"){
    tab <- nipa.a %>%
      ungroup %>%
      filter(tabnum==tabnumc) %>%
      filter(year==max(year)) %>%
      arrange(tabnum, tabname, line) %>%
      select(-tabnum)
  } else if(freq=="Q"){
    tab <- nipa.q %>%
      ungroup %>%
      filter(tabnum==tabnumc) %>%
      filter(date==max(date)) %>%
      arrange(tabnum, tabname, line) %>%
      select(-tabnum)
    }
  return(tab)
}



