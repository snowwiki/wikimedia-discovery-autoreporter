if ("iwclick" %in% events$event) {

  iwclick_position_function <- function(by_wiki = FALSE, ...) {
    events %>%
      keep_where(event == "iwclick") %>%
      mutate(position = ifelse(event_position < 4, safe_ordinals(event_position + 1), "5th or higher")) %>%
      group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "position"))) %>%
      tally %>%
      bar_chart(x = "position", y = "n", x_lab = "Clicked Position", y_lab = "Number of inter-wiki clicks",
                title = paste("Number of inter-wiki clicks by clicked position, test group", switch(by_wiki, "and wiki", NULL)),
                subtitle = "Inter-wiki results are provided by TextCat language detection")
  }
  p <- iwclick_position_function() + wmf::theme_min()
  ggsave("iwclick_position_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
  rm(p)

  if (iwclick_wiki > 1) {
    p <- iwclick_position_function(by_wiki = TRUE) +
      facet_wrap(~ wiki, nrow = n_wiki, scales = "free") +
      wmf::theme_facet()
    ggsave("iwclick_position_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 4 * iwclick_wiki, width = fig_width, limitsize = FALSE)
    rm(p)
  }

}
