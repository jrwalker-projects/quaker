#' Displays points in a horizontal timeline
#'
#' \code{geom_timeline} is a clone of ggplot2 geom_point that can be used to display points from the NOAA earthquake dataset.
#' The 'x' aesthetic is displayed as data points (see supported optional aesthetics below) and the 'optional y' aesthetic can
#' be used to show multiple timelines, for example by selected countries, in a chart.
#'
#' \code{GeomTimeline} is the \code{ggproto} environment object to enable \code{geom_timeline}
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If specified and inherit.aes = TRUE (the default), it is
#' combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:If NULL, the default, the data is inherited from the
#' plot data as specified in the call to \code{ggplot()}.A data.frame, or other object, will override the plot data. All objects will be
#' fortified to produce a data frame. See \code{fortify()} for which variables will be created.A function will be called with a
#' single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param ... other arguments passed on to \code{layer()}. These are often aesthetics, used to set an aesthetic to a fixed value,
#' like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#' FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper
#' functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param xmin minimum year to use in the diaplay. If no value for xmin is passed, no filtring is done within the geom for lower
#' x value dates. If xmin is passed, data rows with x dates below 1 January in xmin year will be filtered out of the display.
#' @param xmax maximum year to use in the diaplay. If no value for xmax is passed, no filtring is done within the geom for higher
#' x value dates. If xmax is passed, data rows with x dates above 31 December in xmax year will be filtered out of the display.
#'
#' @section Aesthetics:
#' geom_timeline understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}
#'}
#' where x is a vector of type date
#'
#' the following aesthetics are optional:
#' \itemize{
#'   \item y (can be used as a categorical variable to chart multiple timelines, for example by country)
#'   \item alpha
#'   \item colour
#'   \item fill
#'   \item group
#'   \item stroke
#'   \item size
#' }
#' @return a ggplot object that can be combined with ggplot or ggmap - this object behaves somewhat
#' like a \code{geom_point} ggplot object
#'
#' @examples
#' \dontrun{
#' my_clean_df %>%
#'   filter(COUNTRY %in% c("GREECE", "FRANCE", "JAPAN")) %>%
#'   ggplot(aes(x=DATE, y=COUNTRY, colour=EQ_PRIMARY, alpha=TOTAL_DEATHS,
#'          size=EQ_PRIMARY, label=LOCATION_NAME)) +
#'   geom_timeline(xmin=-200) +
#'   guides(alpha = FALSE, colour=FALSE) +
#'   scale_x_date(date_labels = "%Y")
#' }
#' @seealso \code{\link{geom_timeline}} to plot the points and \code{\link{eq_clean_data}} to prepare the NOAA data
#'
#' @references for more information on ggplot see http://ggplot2.org/ and http://ggplot2.tidyverse.org/
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          xmin = NA, xmax = NA, #new filter parameters
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE){
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xmin=xmin, xmax=xmax,
      na.rm = na.rm,
      ...
    )
  )
}
#' @rdname geom_timeline
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 draw_key_point
#' @importFrom lubridate year
#' @importFrom dplyr filter
#' @importFrom grid segmentsGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#' @export
GeomTimeline <- ggplot2::ggproto("GeomPoint", ggplot2::Geom,
                        required_aes = c("x"),
                        non_missing_aes = c("size", "shape", "colour"),
                        default_aes = ggplot2::aes(
                          shape = 19, colour = "black", size = 1.5, fill = NA,
                          alpha = NA, stroke = 0.5
                        ),

                        draw_panel = function(data, panel_params, coord, na.rm = FALSE, xmin=NA, xmax=NA) {
                          date_prep <- function(y, m, d){
                            #as.date doesn't process negative years so workaround
                            if (y < 0){yr <- y * -1} else {yr <- y}
                            ymd <- as.Date(paste0(yr, "/", m, "/", d), format='%Y/%m/%d')
                            if (y < 0){
                              lubridate::year(ymd) <- y
                            }
                            return(as.numeric(ymd))
                          }
                          #filter dates by years if xmin or xmax passed
                          if(!is.na(xmin)){
                            ymd_min <- date_prep(xmin,1,1)
                            data <- data %>%
                              dplyr::filter(x >= ymd_min)
                          }
                          if(!is.na(xmax)){
                            ymd_max <- date_prep(xmax,12,31)
                            data <- data %>%
                              dplyr::filter(x <= ymd_max)
                          }
                          if (is.null(data$x)) return(zeroGrob())

                          coords <- coord$transform(data, panel_params)
                          seg_grob <- grid::segmentsGrob(min(coords$x), coords$y, max(coords$x), coords$y,
                                                   default.units = "native",
                                                   gp = grid::gpar(col = "#636363", fill = "#636363", lwd = 1, lty = 1) #line segment grey
                          )
                          #grob code below is adapted from geom_point (github tidyverse\ggplot2)
                          dots_grob <- grid::pointsGrob(
                            coords$x, coords$y,
                            pch = coords$shape,
                            gp = grid::gpar(
                              col = alpha(coords$colour, coords$alpha),
                              fill = alpha(coords$fill, coords$alpha),
                              fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                              lwd = coords$stroke * .stroke / 2
                            )
                          )
                          gTree(children = gList(seg_grob, dots_grob))
                        },

                        draw_key = ggplot2::draw_key_point
)
#' Labels points in a horizontal timeline
#'
#' \code{geom_timeline_label} is a clone of ggplot2 geom_text that can be used to label points from the NOAA earthquake dataset.
#' The 'x' aesthetic identify dates to be labeled on the horizontal asix (see supported optional aesthetics below) and the
#' 'optional y' aesthetic can be used to label multiple timelines, for example by selected countries, in a chart.
#' geom_timeline_label is designed to be an optional addition to geom_timeline.
#'
#' \code{GeomTimelineLabel} is the \code{ggproto} environment object to enable \code{geom_timeline_label}
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or \code{aes_()}. If specified and inherit.aes = TRUE (the default), it is
#' combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:If NULL, the default, the data is inherited from the
#' plot data as specified in the call to \code{ggplot()}.A data.frame, or other object, will override the plot data. All objects will be
#' fortified to produce a data frame. See \code{fortify()} for which variables will be created.A function will be called with a
#' single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param ... other arguments passed on to \code{layer()}. These are often aesthetics, used to set an aesthetic to a fixed value,
#' like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' @param check_overlap logical - TRUE omits text overlapping with other text, FALSE displays all text. Default FALSE
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped.
#' FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper
#' functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param xmin numeric minimum year to use in the diaplay. If no value for xmin is passed, no filtring is done within the geom for lower
#' x value dates. If xmin is passed, data rows with x dates below 1 January in xmin year will be filtered out of the display.
#' @param xmax numeric maximum year to use in the diaplay. If no value for xmax is passed, no filtring is done within the geom for higher
#' x value dates. If xmax is passed, data rows with x dates above 31 December in xmax year will be filtered out of the display.
#' @param n_max numeric if passed is the largest number of points to be labeled, for example if the y aesthetic identifies three
#' countries to be included in the chart and n_max has a value of 3, then the 3 largest earthquakes by size for each country will
#' be labeled. If n_max is not passed then all points are labeled.
#'
#' @section Aesthetics:
#' geom_timeline understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{x}
#'   \item \strong{label}
#' }
#' where x is a vector of type date and label contains the text to be used for the labels
#'
#' the following aesthetics are optional:
#' \itemize{
#'   \item y (can be used as a categorical variable to chart multiple timelines, for example by country)
#'   \item fontface
#'   \item lineheight
#' }
#' @return a ggplot object that can be combined with ggplot or ggmap - this object behaves like a \code{geom_point} ggplot object
#'
#' @examples
#' \dontrun{
#' my_clean_df %>%
#'   filter(COUNTRY %in% c("CHINA", "INDONESIA", "JAPAN")) %>%
#'   filter(lubridate::year(DATE) > 2000) %>%
#'   ggplot(aes(x=DATE, y=COUNTRY, colour=EQ_PRIMARY, alpha=TOTAL_DEATHS,
#'          size=EQ_PRIMARY, label=LOCATION_NAME)) +
#'   geom_timeline() +
#'   geom_timeline_label(n_max = 3) + #label top 3 for each country
#'   guides(alpha = FALSE, colour=FALSE) +
#'   scale_x_date(date_labels = "%Y")
#' }
#' @seealso \code{\link{geom_timeline_label}} which can be used to label some of the points
#'
#' @references for more information on ggplot see http://ggplot2.org/ and http://ggplot2.tidyverse.org/
#'
#' @importFrom ggplot2 layer
#'
#' @export
#label geom partners with geom_timeline
geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                xmin = NA, xmax = NA, n_max = NA,#new filter parameters
                                ...,
                                check_overlap = FALSE,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xmin=xmin, xmax=xmax, n_max = n_max,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}
#' @rdname geom_timeline_label
#' @format NULL
#' @usage NULL
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 draw_key_text
#' @importFrom lubridate year
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr top_n
#' @importFrom dplyr ungroup
#' @importFrom grid segmentsGrob
#' @importFrom grid textGrob
#' @importFrom grid gpar
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomText", ggplot2::Geom,
                             required_aes = c("x", "label"),
                             default_aes = ggplot2::aes(
                               colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                               vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
                             ),

                             draw_panel = function(data, panel_params, coord, na.rm = FALSE, check_overlap = FALSE, xmin=NA, xmax=NA, n_max=NA) {
                               date_prep <- function(y, m, d){
                                 #for now as.date doesn't process negative years so workaround
                                 if (y < 0){yr <- y * -1} else {yr <- y}
                                 ymd <- as.Date(paste0(yr, "/", m, "/", d), format='%Y/%m/%d')
                                 if (y < 0){
                                   lubridate::year(ymd) <- y
                                 }
                                 return(as.numeric(ymd))
                               }
                               #filter dates by years if xmin or xmax passed
                               if(!is.na(xmin)){
                                 ymd_min <- date_prep(xmin,1,1)
                                 data <- data %>%
                                   dplyr::filter(x >= ymd_min)
                               }
                               if(!is.na(xmax)){
                                 ymd_max <- date_prep(xmax,12,31)
                                 data <- data %>%
                                   dplyr::filter(x <= ymd_max)
                               }
                               if(!is.na(n_max)){ #filter max items by size
                                 data <- data %>%
                                   dplyr::group_by(group) %>%
                                   dplyr::top_n(n_max, size) %>%
                                   dplyr::ungroup()
                               }
                               if (is.null(data$x)) return(grid::zeroGrob())

                               coords <- coord$transform(data, panel_params)
                               seg_grob <- grid::segmentsGrob(coords$x, coords$y, coords$x, (coords$y*1.2),
                                                        default.units = "native",
                                                        gp = grid::gpar(col = "#636363", fill = "#636363", lwd = 0.8, lty = 1) #line segment grey
                               )
                               txt_grob <- grid::textGrob(
                                 data$label,
                                 coords$x, coords$y*1.2, default.units = "native",
                                 #      just="right",
                                 hjust = 0, vjust = 0.6, #left justified & raised slightly above center
                                 rot = 45,
                                 gp = grid::gpar(
                                   col = "black",
                                   fontsize=12,
                                   fontface = data$fontface,
                                   lineheight = data$lineheight
                                 ),
                                 check.overlap = TRUE #don't write overlapping text
                               )
                               grid::gTree(children = gList(seg_grob, txt_grob))
                             },

                             draw_key = ggplot2::draw_key_text
)
