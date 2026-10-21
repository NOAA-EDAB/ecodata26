ecodata26::trans_dates %>%
  dplyr::filter(
    EPU %in% c("GB", "GOM"),
    Var %in% c("falltrans", "sprtrans"),
    !Value == "NA",
    !Var == "NA",
    !Time == "NA"
  ) %>%
  dplyr::mutate(
    Var = recode(
      Var,
      "falltrans" = "Fall",
      "sprtrans" = "Spring",
      "maxday" = "Max"
    )
  ) %>%
  ggplot(aes(x = Time, y = Value, color = Var)) +
  geom_point() +
  geom_line() +
  ecodata26::geom_gls() +
  ggplot2::theme(
    strip.text = element_text(hjust = 0),
    plot.title = element_text(size = 12)
  ) +
  ecodata26::theme_title() +
  ylab("Day of Year") +
  ggplot2::facet_wrap(. ~ EPU) +
  ecodata26::theme_facet() +
  xlab(element_blank())
