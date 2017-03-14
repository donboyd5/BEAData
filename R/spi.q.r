#' BEA Regional state personal income data, quarterly.
#'
#' State personal income components and selected other variables, various years.
#'
#' @source Bureau of Economic Analysis
#' @format Data frame with 1 row per quarter, per state, per variable.
#' \describe{
#' \item{stabbr}{state abbreviation, factor}
#' \item{date}{First day of quarter, date}
#' \item{line}{BEA line number (i.e., expenditure category), integer}
#' \item{vname}{Short variable name I created, character}
#' \item{description}{BEA description associated with the line number, character}
#' \item{value}{Value, numeric}
#' }
#' @examples
#'   spi.q
#'   comment(spi.q)
"spi.q"

