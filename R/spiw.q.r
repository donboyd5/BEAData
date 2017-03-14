#' BEA Regional state personal income data, wide, quarterly.
#'
#' State personal income components and selected other variables, wide, various years.
#'
#' @source Bureau of Economic Analysis
#' @format Data frame with 1 row per state and quarter.
#' \describe{
#' \item{stabbr}{state abbreviation, factor}
#' \item{date}{First day of quarter, date}
#' \item{...}{One column per variable in the long file, numeric}
#' }
#' @examples
#'   spiw.q
#'   comment(spiw.q)
"spiw.q"


