#' Example NOAA earthquake data
#'
#' A dataset containing 64 rows of the NOAA earthquake dataset to provide sample data for the quaker routines
#'
#' @format A table data frame with 64 rows of 47 variables (see the data definition reference below for full description):
#' \describe{
#'   \item{I_D}{integer row ID}
#'   \item{FLAG_TSNAMI}{character - contains "Tsu" where these was a tsunami event}
#'   \item{YEAR}{integer year of the quake - can be negative to denote B.C.E year}
#'   ...
#' }
#' @examples
#' \dontrun{
#' data(quakes)
#' my_clean_df <- eq_clean_data(quakes)
#' }
#' @seealso \code{\link{eq_clean_data}} to prepare the NOAA data for plotting or mapping functions

#' @source \url{https://www.ngdc.noaa.gov/nndc/struts/results?&t=101650&s=225&d=225} data definitions - the data can be
#' downloaded from \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
"quakes"
