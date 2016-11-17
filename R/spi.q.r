#' BEA Regional state personal income data, quarterly.
#'
#' State personal income components and selected other variables, various years.
#'
#' @source Bureau of Economic Analysis
#' @format Data frame with 1 row per quarter.
#' \describe{
#' \item{stabbr}{state abbreviation, factor}
#' \item{date}{First day of quarter, date}
#' \item{line}{BEA line number (i.e., expenditure category), integer}
#' \item{description}{BEA description associated with the line number, character}
#' \item{value}{Value, numeric}
#' }
#' @examples
#'   spi.q
"spi.q"
"comment(spi.q)"

