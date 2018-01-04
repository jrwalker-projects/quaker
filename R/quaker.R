#' Clean, plot and map NOAA earthquake data.
#'
#' the \code{quaker} package provides a set of functions to clean and prepare U.S. National Oceanographic and
#' Atmospheric Administration (NOAA) earthquake data, plot a timeline of earthquakes or map earthquake locations.
#'
#'@references
#' The NOAA earthquake database https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1 and the data defintions
#' can be found at https://www.ngdc.noaa.gov/nndc/struts/results?&t=101650&s=225&d=225
#'
#' The approach used for cleaning and plotting data is that of the R tidyverse - more on this topic can be found at
#' https://www.tidyverse.org/ where useful aspects might be \code{dplyr} for pipelines and data manipulation and
#' \code{ggplot2} for charts - including the ggplot2 extension geoms included in this package.
#'
#' The mapping functions are based on the R package \code{leaflet}. More on this remarkable mapping tool can be
#' found at https://rstudio.github.io/leaflet/
#'
#' @section Functions:
#' that might be of interest from \pkg{quaker} include\cr
#' \code{\link{eq_clean_data}} to clean and prepare the NOAA data after it has been read from the website\cr
#' \code{\link{geom_timeline}} to plot earthquake activity over time\cr
#' \code{\link{geom_timeline_label}} to annotate a geom_timeline plot with labels\cr
#' \code{\link{eq_map}} to map earthquake locations\cr
#' Please refer to the vignette for more information.
"_PACKAGE"
#> [1] "_PACKAGE"
