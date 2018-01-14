#' Clean NOAA earthquake data
#'
#' \code{eq_clean_data} cleans the NOAA earthquake data from the website and returns a data frame for use in analysis
#' and visualization. A column DATE is created from YEAR, MONTH and DAY values. Numeric fields are set to be numeric,
#' for example the different earthquake magnitude values.
#'
#' @param indf is the raw NOAA data frame as read from the web site - a link to the data source and to the data descriptions
#' can be found below in the refereces
#'
#' @return the cleaned data frame
#'
#' @details
#' The DATE column is created from the YEAR, MONTH and DAY values (which are removed). If only the YEAR is supplied, the date
#' is set to be 1 January for that year. Note that some dates may have a negative year value indicating a B.C.E date.
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%                #read the data file
#'   eq_clean_data() %>%                                                    #clean the data
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% #select for analysis
#'   eq_map(annot_col = "DATE")                                             #map the locations
#' }
#' @references
#' The NOAA earthquake database https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1 and the data defintions
#' can be found at https://www.ngdc.noaa.gov/nndc/struts/results?&t=101650&s=225&d=225
#'
#' @importFrom tidyr separate
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate year
#' @importFrom tools toTitleCase
#' @importFrom stringr str_trim
#' @importFrom magrittr %>%
#'
#' @export
eq_clean_data <- function(indf){
  outdf <- indf %>%
    tidyr::separate(col=LOCATION_NAME, into=c("Country", "LOCATION_NAME"), sep=":", fill="left", extra="merge") %>%
    dplyr::select(-Country) %>% #remove col created from separate
    dplyr::mutate(MONTH = as.integer(ifelse(is.na(MONTH), 1, MONTH)),
                  DAY = as.integer(ifelse(is.na(DAY), 1, DAY)),
                  yy = as.integer(ifelse(YEAR < 0, (YEAR*-1), YEAR)),
                  DATE = as.Date(paste0(yy, "/", MONTH, "/", DAY), format = '%Y/%m/%d'),
                  LOCATION_NAME = tools::toTitleCase(tolower(LOCATION_NAME)),
                  LOCATION_NAME = stringr::str_trim(LOCATION_NAME),
                  LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE),
                  EQ_PRIMARY = ifelse(is.na(EQ_PRIMARY), as.numeric(0), as.numeric(EQ_PRIMARY)),
                  EQ_MAG_MW = ifelse(is.na(EQ_MAG_MW), as.numeric(0), as.numeric(EQ_MAG_MW)),
                  EQ_MAG_MS = ifelse(is.na(EQ_MAG_MS), as.numeric(0), as.numeric(EQ_MAG_MS)),
                  EQ_MAG_MB = ifelse(is.na(EQ_MAG_MB), as.numeric(0), as.numeric(EQ_MAG_MB)),
                  EQ_MAG_ML = ifelse(is.na(EQ_MAG_ML), as.numeric(0), as.numeric(EQ_MAG_ML)),
                  EQ_MAG_MFA = ifelse(is.na(EQ_MAG_MFA), as.numeric(0), as.numeric(EQ_MAG_MFA)),
                  REGION_CODE = as.factor(REGION_CODE),
                  COUNTRY = as.factor(COUNTRY),
                  TOTAL_DEATHS = ifelse(is.na(TOTAL_DEATHS), as.numeric(0), as.numeric(TOTAL_DEATHS)))
  lubridate::year(outdf$DATE) <- outdf$YEAR
  outdf <- outdf %>%
    dplyr::select(-YEAR, -MONTH, -DAY, -yy)
  return(outdf)
}
