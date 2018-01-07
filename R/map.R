#' Map NOAA earthquake data
#'
#' \code{eq_map} displays an interactive map showing earthquake locations from the NOAA earthquake data. \code{eq_map} uses
#' the leaflet R package to
#'
#' @param df is the cleaned NOAA data frame after \code{eq_clean_data} has been called - a link to the data source and to the
#' data descriptions can be found below in the refereces
#' @param annot_col is a single text value for the column name to be displayed as the popup value when the user hovers over
#' a data point on the map. This column can be a single data item, for example "DATE" or this can be a column of combined
#' values using the \code{eq_create_label} function.
#' @param pretty single logical value. If pretty is true, the columne name of 'annot_col' is displayed prior to the value
#' in the popup. If pretty is FALSE or not passed, only the value is displayed.
#'
#' @return the cleaned NOAA data frame
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#'  eq_clean_data() %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  eq_map(annot_col = "DATE", pretty = TRUE)
#' }
#' @seealso \code{\link{eq_create_label}} to create popup display with multiple columns and
#' \code{\link{eq_clean_data}} to prepare the NOAA data
#' @references
#' The NOAA earthquake database https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1 and the data defintions
#' can be found at https://www.ngdc.noaa.gov/nndc/struts/results?&t=101650&s=225&d=225
#'
#' @importFrom tools toTitleCase
#' @importFrom dplyr select
#' @importFrom dplyr one_of
#' @importFrom dplyr mutate
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom magrittr %>%
#'
#' @export
eq_map <- function(df, annot_col, pretty = FALSE){
  ptxt <- tools::toTitleCase(base::tolower(annot_col))
  tf <- df %>%
    dplyr::select(dplyr::one_of(annot_col))
  names(tf) <- "popup_info"
  df$popup_info <- tf$popup_info
  if(pretty){
    #pretty adds the column name in front of the popup value
    df <- df %>%
      dplyr::mutate(popup_info = paste("<b>", ptxt, ":</b> ", popup_info))
  }
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = df, radius = ~ EQ_PRIMARY,
                     lng = ~ LONGITUDE, lat = ~ LATITUDE,
                     popup = ~ popup_info)
}
#' Map NOAA earthquake data
#'
#' \code{eq_create_label} builds a vector combining location, magnitude and deaths for display in map popups
#'
#' @param df is the cleaned NOAA data frame after \code{eq_clean_data} has been called - a link to the data source and to the
#' data descriptions can be found below in the refereces
#' @return a character vector with the combined information location, magnitude and total deaths. This vector can be added
#' to the earthquake data frame to enhance popup display
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
#' @seealso \code{\link{eq_map}} to create the map of earthquake locations and
#' \code{\link{eq_clean_data}} to prepare the NOAA data
#' @references
#' The NOAA earthquake database https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1 and the data defintions
#' can be found at https://www.ngdc.noaa.gov/nndc/struts/results?&t=101650&s=225&d=225
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @export
eq_create_label <- function(df){
  tf <- df %>%
    dplyr::mutate(loc = ifelse(is.na(LOCATION_NAME), "", paste("<b>Location: </b>", LOCATION_NAME, "<br />")),
           mag = ifelse(is.na(EQ_PRIMARY), "", paste("<b>Magnitude: </b>", EQ_PRIMARY, "<br />")),
           dead = ifelse(is.na(TOTAL_DEATHS), "", paste("<b>Total Deaths: </b>", TOTAL_DEATHS)))
  return(paste0(tf$loc, tf$mag, tf$dead))
}
