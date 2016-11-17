#' BEA Regional state personal consumption expenditure data, annual.
#'
#' State personal consumption expenditures, annual, various years.
#'
#' @source Bureau of Economic Analysis
#' @format Data frame with 1 row per year.
#' \describe{
#' \item{stabbr}{state abbreviation, factor}
#' \item{year}{Calendar year, integer}
#' \item{line}{BEA line number (i.e., expenditure category), integer}
#' \item{description}{BEA description associated with the line number, character}
#' \item{value}{State personal consumption expenditures for this line number, nominal, $ billions, numeric}
#' }
#' @examples
#'   spce.a
"spce.a"
"comment(spce.a)"

