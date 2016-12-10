#' National Income and Products Accounts, quarterly.
#'
#' NIPA, quarterly, various years.
#'
#' @source Bureau of Economic Analysis
#' @format Data frame with 1 row per variable per quarter.
#' \describe{
#' \item{tabnum}{NIPA table number, character}
#' \item{tabname}{NIPA table name, character}
#' \item{line}{Line number in table, integer}
#' \item{vdesc}{Variable description, character}
#' \item{vname}{BEA variable name, character}
#' \item{date}{First day of quarter, date}
#' \item{value}{Units depend upon the table in question, numeric}
#' }
#' @examples
#' require("BEAData")
#' nipa.q
#' comment(nipa.q)
"nipa.q"

