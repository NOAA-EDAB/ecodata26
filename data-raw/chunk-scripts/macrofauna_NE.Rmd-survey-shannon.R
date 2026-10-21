ecodata26::survey_shannon %>%
  filter(!EPU == "MAB", !EPU == "SS") %>%
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ecodata26::geom_gls() +
  #ecodata26::geom_lm()+
  ggtitle("Survey Shannon Diversity Index") +
  ggplot2::facet_wrap(~EPU) +
  ggplot2::ylab("Shannon") +
  ggplot2::xlab(element_blank()) +
  ggplot2::scale_color_discrete(name = "Season", labels = c("Fall", "Spring")) +
  ecodata26::theme_ts() +
  ecodata26::theme_title()
