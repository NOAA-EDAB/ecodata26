#' plot bottom anomaly temperature time series using NEFSC survey data
#'
#' ecodata26::bottom_temp_insitu
#'
#' @param shadedRegion Numeric vector. Years denoting the shaded region of the plot (most recent 10)
#' @param report Character string. Which SOE report ("MidAtlantic", "NewEngland")
#' @param n Numeric scalar. Number of years used (from most recent year) to estimate short term trend . Default = 0 (No trend calculated)
#'
#'
#' @return ggplot object
#'
#' @export

plot_bottom_temp_insitu <- function(
  shadedRegion = NULL,
  report = "MidAtlantic",
  n = 0
) {
  setup <- ecodata26::plot_setup(shadedRegion = shadedRegion, report = report)
  if (report == "MidAtlantic") {
    filterEPUs <- c("MAB")
  } else {
    filterEPUs <- c("GB", "GOM")
  }

  fix <- ecodata26::bottom_temp_insitu |>
    dplyr::filter(EPU %in% filterEPUs) |>
    tidyr::complete(
      Time = tidyr::full_seq(
        min(ecodata26::bottom_temp_insitu$Time):max(
          ecodata26::bottom_temp_insitu$Time
        ),
        1
      ),
      tidyr::nesting(Var)
    ) |>
    dplyr::mutate(hline = 0) |>
    dplyr::filter(Var == "bottom temp anomaly in situ", !is.na(Value))

  #gl_bt<- ecodata26::bottom_temp_glorys |>
  #  dplyr::filter(EPU %in% filterEPUs)

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
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~EPU) +
    ggplot2::geom_hline(yintercept = 0, linetype = setup$hline.lty) +
    # ggplot2::geom_point(ggplot2::aes(x = gl_bt$Time, y = gl_bt$Value), size = 1, color = "red") +
    #  ggplot2::geom_line(ggplot2::aes(x = gl_bt$Time, y = gl_bt$Value), color = "red") +
    #ecodata26::geom_lm(aes(x = temp_anom$Time, y = temp_anom$Value))+
    ggplot2::ylab("Temperature (C)") +
    ggplot2::xlab(ggplot2::element_blank()) +
    ggplot2::ggtitle(paste(
      report,
      ": NEFSC survey bottom temperature anomaly"
    )) +
    ecodata26::geom_gls() +
    ecodata26::geom_lm(n = n) +
    ecodata26::theme_ts() +
    ecodata26::theme_facet() +
    ecodata26::theme_title()

  return(p)
}


attr(plot_bottom_temp_insitu, "report") <- c("MidAtlantic", "NewEngland")
