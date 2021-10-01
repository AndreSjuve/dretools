
# add_small_lengend() ----------------------------------------------------------

#' Add small legend to ggplot
#'
#' \code{add_small_legend} is a function to adjust the size of a plot legend.
#' Used in scripts: {}
#' Robust function: TRUE
#'
#' @param gg_plot plot of ggplot type
#'
#' @param point_size Size of legend pointers
#'
#' @param text_size Size of legend text
#'
#' @param space_legend Space between legends
#'
#' @return Plot with adjusted legend
#' @export
#'
#' @importFrom ggplot2 guides theme guide_legend element_text unit

add_small_legend <-
  function(gg_plot, point_size = 0.5, text_size = 3, space_legend = 0.1) {
    gg_plot +
      guides(shape = guide_legend(override.aes = list(size = point_size)),
             color = guide_legend(override.aes = list(size = point_size))) +
      theme(legend.title = element_text(size = text_size),
            legend.text  = element_text(size = text_size),
            legend.key.size = unit(space_legend, "lines"))
  }


# theme_wss_latex() ------------------------------------------------------------

#' Custom Latex theme
#'
#' \code{theme_wss_latex} is a custom theme for figures that will be used in
#' latex files, i.e. research papers. The theme is quite minimalistic and clean
#' as figures used in research should be. The theme is prone to adjustments in
#' the future
#'
#' @param base_size The standard size for text in the figure
#'
#' @param base_family Left open
#'
#' @importFrom extrafont loadfonts
#' @importFrom ggplot2 theme theme_grey element_line element_rect margin rel element_blank
#'
#' @return Returns the same figure, only with a new theme
#'
#' @export
#'
theme_wss_latex <- function(base_size = 10, base_family = "") {
  sky_blue <- "#EFF6F9"
  nhh_blue <- "#00395A"
  nhh_grey <- "#919195"
  white    <- "#FFFFFF"

  '%+replace%' <- ggplot2::'%+replace%' # nolint

  ggplot2::theme_grey(base_size = base_size, base_family = base_family)
  '%+replace%'
  ggplot2::theme(line = ggplot2::element_line(colour   = "black",
                                              size     = 0.5,
                                              linetype = 1,
                                              lineend  = "butt"),
                 rect = ggplot2::element_rect(fill     = white,
                                              colour   = "black",
                                              size     = 0.5,
                                              linetype = 1),
                 text = ggplot2::element_text(family   = base_family,
                                              face     = "plain",
                                              colour   = "black",
                                              size     = base_size,
                                              lineheight = 0.9,
                                              hjust      = 0.5,
                                              vjust      = 0.5,
                                              angle      = 0,
                                              margin     = ggplot2::margin(),
                                              debug      = FALSE),
                 axis.line = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size   = ggplot2::rel(0.8)),
                 axis.ticks = ggplot2::element_line(color = nhh_grey,
                                                    size  = ggplot2::rel(1/3)),
                 axis.title = ggplot2::element_text(size  = ggplot2::rel(1)),
                 panel.background = ggplot2::element_rect(fill = sky_blue),
                 plot.background = ggplot2::element_rect(fill  = "white",
                                                         color = NA),
                 panel.border = ggplot2::element_rect(fill = NA,
                                                      size = rel(1/2),
                                                      color = nhh_grey),
                 panel.grid.major = ggplot2::element_line(color = white,
                                                          size  = ggplot2::rel(1/3)),
                 panel.grid.minor = ggplot2::element_line(color = white,
                                                          size  = ggplot2::rel(1/3)),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.spacing = ggplot2::unit(0.75, "cm"),
                 legend.key = ggplot2::element_rect(fill = white,
                                                    color = NA),
                 legend.position = "bottom",
                 strip.background = ggplot2::element_rect(fill = nhh_blue,
                                                          color = nhh_blue),
                 strip.text = ggplot2::element_text(color = white,
                                                    size  = ggplot2::rel(0.8)),
                 plot.title = ggplot2::element_text(size = ggplot2::rel(1.2),
                                                    hjust = 0,
                                                    margin = ggplot2::margin(
                                                      t = 0, r = 0,
                                                      b = 4, l = 0,
                                                      unit = "pt")),
                 plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.9),
                                                       hjust = 0,
                                                       margin = ggplot2::margin(
                                                         t = 0, r = 0,
                                                         b = 3, l = 0,
                                                         unit = "pt")),
                 complete = TRUE)



}


