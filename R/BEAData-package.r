#' Data from the U.S. Bureau of Economic Analysis (BEA).
#'
#' Includes key national and state-level regional BEA data, annual and quarterly.
#'
#' \bold{Currently includes:}\cr
#'
#' National data files:
#' \tabular{lll}{
#' \code{\link{nipa.a}}  \tab  \tab National Income and Products Accounts, annual\cr
#' \code{\link{nipa.au}} \tab   \tab National Income and Products Accounts, annual, unique variables\cr
#' \code{\link{nipa.q}} \tab   \tab National Income and Products Accounts, quarterly\cr
#' \code{\link{nipa.qu}} \tab   \tab National Income and Products Accounts, quarterly, unique variables\cr
#' }
#'
#' Functions for national data:
#' \tabular{lll}{
#' \code{\link{getgdppi}}  \tab  \tab Get a data frame with the GDP price index, either annual or quarterly.\cr
#' \code{\link{listtabs}} \tab   \tab Get a list of NIPA.A or NIPA.Q table numbers and descriptions.\cr
#' \code{\link{showtab}} \tab   \tab Show values in a NIPA.A or NIPA.Q table, in line order, for the latest year or quarter.\cr
#' }
#'
#' State data files:
#' \tabular{lll}{
#' \code{\link{sgdp.a}} \tab   \tab  State GDP data, annual\cr
#' \code{\link{sgdp.a_all}} \tab   \tab  State GDP data all components, annual\cr
#' \code{\link{sgdp.q}} \tab   \tab  State GDP data, quarterly\cr
#' \code{\link{sgdp.q_all}} \tab   \tab  State GDP data all components, quarterly\cr\cr
#'
#' \code{\link{spi.a}} \tab   \tab  State personal income data, annual\cr
#' \code{\link{spi.q}} \tab   \tab  State personal income data, quarterly\cr\cr
#'
#' \code{\link{spce.a}} \tab   \tab  State personal consumption expenditure data, annual\cr
#' }
#'
#'
#' @docType package
#' @name BEAData
NULL

