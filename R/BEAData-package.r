#' Data from the U.S. Bureau of Economic Analysis (BEA).
#'
#' Includes key national and state-level regional BEA data, annual and quarterly.
#'
#' \bold{Currently includes:}\cr
#'
#' National data files:
#' \tabular{lll}{
#' \code{\link{nipa}}  \tab  \tab National Income and Products Accounts, annual, quarterly, and monthly\cr
#' \code{\link{NIPAvars}}  \tab  \tab Information on variables in the National Income and Products Accounts\cr
#' }
#'
#' Functions for national data:
#' \tabular{lll}{
#' \code{\link{getgdppi}}  \tab  \tab Get a data frame with the GDP price index, either annual or quarterly.\cr
#' \code{\link{getNIPATable}} \tab   \tab Return a NIPA table descriptive information as a data frame.\cr
#' \code{\link{getNIPAvarinfo}} \tab   \tab Get a data frame with information on a variable in a specific BEA table and line number.\cr
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

