#' National Income and Products Accounts, annual, quarterly, and monthly, all data.
#'
#' NIPA, annual, quarterly, and monthly, all data.
#'
#' @source Bureau of Economic Analysis
#' @format Data frame with 1 row per variable per date.
#' \describe{
#' \item{vname}{BEA variable name; character}
#' \item{date}{first day of year (annual), first day of quarter (quarterly), first day of month (monthly); date}
#' \item{year}{Calendar year of data; integer}
#' \item{freq}{frequency: A, Q, or M; character}
#' \item{value}{Units depend upon the table in question, numeric}
#' \item{vdesc}{Variable description, character}
#' }
#' @examples
#' nipa
#' comment(nipa)
"nipa"

