#' National Income and Products Accounts, annual.
#'
#' NIPA, annual, various years.
#'
#' @source Bureau of Economic Analysis
#' @format Data frame with 1 row per variable per year.
#' \describe{
#' \item{tabnum}{NIPA table number, character}
#' \item{tabname}{NIPA table name, character}
#' \item{line}{Line number in table, integer}
#' \item{vdesc}{Variable description, character}
#' \item{vname}{BEA variable name, character}
#' \item{year}{Calendar year of data, integer}
#' \item{value}{Units depend upon the table in question, numeric}
#' }
#' @examples
#' require("BEAData")
#' nipa.a
#' comment(nipa.a)
#'
"nipa.a"

