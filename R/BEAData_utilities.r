

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


