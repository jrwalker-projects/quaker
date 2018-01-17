#' .onLoad function to resolve R CMD check note
#'
#' Package checking in R - R CMD check - doesn't currently deal with a function that passes a data frame and
#' uses explicit column names. Passing variables as quoted or quo/enquo is possible but in the case of a routine
#' like eq_clean_data that only ever intends to deal with one data frame, explicit column names seem acceptable.
#' The R CMD check can be satisfied with the list of column names in a call to GlobalVariables.
#' @param libname the library to be used - default "quaker"
#' @param pkgname the name of the package - default "quaker"
#' @return NULL
#' @note this function is note exported to the package
#' @examples .onLoad()
.onLoad <- function(libname = find.package("quaker"), pkgname = "quaker"){

  # CRAN check Note avoidance
  if(getRversion() >= "3.1.0")

    utils::globalVariables(
#    utils::suppressForeignCheck(
      #column name references to skip checking
      c("COUNTRY", "Country", "DAY", "EQ_MAG_MB", "EQ_MAG_MFA", "EQ_MAG_ML", "EQ_MAG_MS",
         "EQ_MAG_MW", "EQ_PRIMARY", "LATITUDE", "LOCATION_NAME", "LONGITUDE", "MONTH",
         "REGION_CODE", "TOTAL_DEATHS", "YEAR", "popup_info", "yy"
      )
    )
  invisible()
}
#.onUnload <- function (libpath) {
#  library.dynam.unload("quaker", libpath)
#}
