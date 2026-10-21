a <- ecodata26::ppr %>%
  dplyr::filter(Var == "PPR") %>%
  dplyr::group_by(EPU) %>%
  dplyr::mutate(hline = mean(Value)) %>%
  dplyr::filter(EPU != "MAB", Time <= 1997)

ecodata26::ppr %>%
  dplyr::filter(Var == "PPR") %>%
  dplyr::group_by(EPU) %>%
  dplyr::mutate(hline = mean(Value)) %>%
  dplyr::filter(EPU != "MAB", Time >= 1997) %>%
  ggplot2::ggplot() +
  ggplot2::annotate(
    "rect",
    fill = shade.fill,
    alpha = shade.alpha,
    xmin = x.shade.min,
    xmax = x.shade.max,
    ymin = -Inf,
    ymax = Inf
  ) +
  ggplot2::geom_point(aes(x = Time, y = Value)) +
  ggplot2::geom_line(aes(x = Time, y = Value)) +
  # ecodata26::geom_lm(aes(x = Time, y = Value,
  #              group = Var))+
  ggplot2::geom_line(data = a, aes(x = Time, y = Value), linetype = "dashed") +
  ggplot2::geom_hline(
    aes(yintercept = hline),
    size = hline.size,
    alpha = hline.alpha,
    linetype = hline.lty
  ) +
  ggplot2::scale_x_continuous(expand = c(0.05, 0.05)) +
  ggplot2::facet_wrap(~EPU) +
  ggplot2::ggtitle("Primary Production Required") +
  ggplot2::ylab("Proportion of Total PPD") +
  ggplot2::xlab(element_blank()) +
  ecodata26::theme_ts() +
  ecodata26::theme_title() +
  ecodata26::theme_facet()
