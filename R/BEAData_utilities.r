

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
getgdppi <- function(freq="A"){
  requireNamespace("dplyr", quietly = TRUE)
  if(freq=="A") {
    gdppi <- nipa.a %>% filter(tabnum=="1.1.4", vname=="A191RG3") %>%
      select(year, gdppi=value)
  } else if(freq=="Q"){
    gdppi <- nipa.q %>% filter(tabnum=="1.1.4", vname=="A191RG3") %>%
      select(date, gdppi=value)
  }
  return(gdppi)
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


#' Show values in a NIPA.A or NIPA.Q table, for the latest year or quarter.
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



