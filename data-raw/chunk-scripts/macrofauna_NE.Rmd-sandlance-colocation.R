ecodata26::sandlance %>%
  dplyr::filter(Var %in% c("Sandlance", "Humpback", "GreatShearwater")) %>%
  ggplot() +
  ggplot2::geom_point(aes(x = Time, y = Value, color = Var)) +
  ggplot2::geom_line(aes(x = Time, y = Value, color = Var)) +
  ggplot2::ggtitle("Sandlance") +
  ggplot2::ylab("Number of Individuals") +
  ggplot2::xlab(element_blank()) +
  ecodata26::theme_ts() +
  ecodata26::theme_title() +
  ggplot2::theme(legend.title = element_blank(), legend.position = "bottom") +
  ecodata26::theme_facet()
