#' BEA Regional GDP data all components, quarterly.
#'
#' Regional, quarterly all components, various years.
#'
#' @source Bureau of Economic Analysis
#' @format Data frame with 1 row per industry per component per quarter.
#' \describe{
#' \item{stabbr}{state abbreviation, factor}
#' \item{date}{First day of quarter, date}
#' \item{component}{BEA numeric code, integer}
#' \item{compname}{BEA name, character}
#' \item{ind}{BEA industry code, integer}
#' \item{indclass}{BEA industry classification, character}
#' \item{indname}{BEA industry name, character}
#' \item{value}{Amount, numeric}
#' }
#' @examples
#'   sgdp.q_all
"sgdp.q_all"
"comment(sgdp.q_all)"

