#' plot flow rate of hudson river
#'
#' Plot hudson_river_flow data set
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#' @return ggplot object
#'
#'
#' @export
#'

plot_hudson_river_flow <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  n = 0
) {
  # generate plot setup list (same for all plot functions)
  setup <- ecodata26::plot_setup(shadedRegion = shadedRegion, report = report)

  # which report? this may be bypassed for some figures
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  # optional code to wrangle ecodata26 object prior to plotting
  # e.g., calculate mean, max or other needed values to join below
  fix <- ecodata26::hudson_river_flow |>
    dplyr::filter(Var == "Hudson_meanflow")

  # code for generating plot object p
  # ensure that setup list objects are called as setup$...
  # e.g. fill = setup$shade.fill, alpha = setup$shade.alpha,
  # xmin = setup$x.shade.min , xmax = setup$x.shade.max
  #
  p <- fix |>
    ggplot2::ggplot(ggplot2::aes(x = Time, y = Value)) +
    ggplot2::annotate(
      "rect",
      fill = setup$shade.fill,
      alpha = setup$shade.alpha,
      xmin = setup$x.shade.min,
      xmax = setup$x.shade.max,
      ymin = -Inf,
      ymax = Inf
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ggtitle("Hudson River flow") +
    ggplot2::ylab(expression("mean flowrate (m"^3 * " s"^-1 * ")")) +
    ggplot2::xlab(ggplot2::element_blank()) +
    ecodata26::geom_gls() +
    ecodata26::geom_lm(n = n) +
    ecodata26::theme_ts() +
    ecodata26::theme_facet() +
    ecodata26::theme_title()

  # optional code for New England specific (2 panel) formatting
  if (report == "NewEngland") {
    p <- p +
      ggplot2::theme(
        legend.position = "bottom",
        legend.title = ggplot2::element_blank()
      )
  }

  return(p)
}

attr(plot_hudson_river_flow, "report") <- c("MidAtlantic", "NewEngland")

# Paste commented original plot code chunk for reference
# ecodata26::dataset |>
#   dplyr::filter(Var %in% c("..."),
#                 EPU == "...") |>
#   ... more dataset wrangling as necessary |>
#   ggplot2::ggplot(aes(x = Time, y = Mean, group = Season))+
#   ggplot2::annotate("rect", fill = shade.fill, alpha = shade.alpha,
#                     xmin = x.shade.min , xmax = x.shade.max,
#                     ymin = -Inf, ymax = Inf) +
#   ggplot2::geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Season), alpha = 0.5)+
#   ggplot2::geom_point()+
#   ggplot2::geom_line()+
#   ggplot2::ggtitle("Title")+
#   ggplot2::ylab(expression("Y label"))+
#   ggplot2::xlab(element_blank())+
#   ecodata26::geom_gls()+
#   ecodata26::theme_ts()+
#   ecodata26::theme_title()
#
#
