#' BEA Regional GDP data all components, annual.
#'
#' Regional, annual all components, various years.
#'
#' @source Bureau of Economic Analysis
#' @format Data frame with 1 row per industry per component per year.
#' \describe{
#' \item{stabbr}{state abbreviation, factor}
#' \item{year}{Calendar year, integer}
#' \item{component}{BEA numeric code, integer}
#' \item{compname}{BEA name, character}
#' \item{ind}{BEA industry code, integer}
#' \item{indclass}{BEA industry classification, character}
#' \item{indname}{BEA industry name, character}
#' \item{value}{Amount, numeric}
#' }
#' @examples
#'   sgdp.a_all
"sgdp.a_all"

