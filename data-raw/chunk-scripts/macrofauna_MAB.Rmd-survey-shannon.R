ecodata26::survey_shannon %>%
  filter(EPU == "MAB") %>%
  ggplot2::ggplot(aes(x = Time, y = Value, color = Var)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ecodata26::geom_gls() +
  #ecodata26::geom_lm(aes(x = Time, y = Value, group = Var))+
  ggtitle("MAB Survey Shannon Diversity Index") +
  ggplot2::ylab("Shannon") +
  ggplot2::scale_color_discrete(name = "Season", labels = c("Fall", "Spring")) +
  ecodata26::theme_ts()
