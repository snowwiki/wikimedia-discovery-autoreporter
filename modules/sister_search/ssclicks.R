# ssclicks by project
if (exists("ssclick_des")){

  ssclick_project_function <- function(by_wiki = FALSE, ...) {
    ssclick_des %>%
      dplyr::left_join(events, by = c("session_id", "event_id")) %>%
      group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "project"))) %>%
      tally %>%
      bar_chart(x = "project", y = "n", x_lab = "Project", y_lab = "Number of Sister Search Clicks",
                title = paste("Number of sister search clicks by project, test group", switch(by_wiki, "and wiki", NULL)))
  }
  p <- ssclick_project_function() + wmf::theme_min()
  ggsave("ssclick_project_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
  rm(p)

  if (n_wiki > 1) {
    p <- ssclick_project_function(by_wiki = TRUE) +
      facet_wrap(~ wiki, nrow = n_wiki, scales = "free") +
      wmf::theme_facet()
    ggsave("ssclick_project_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 5 * n_wiki, width = fig_width)
    rm(p)
  }

}

# ssclicks by position
if ("ssclick" %in% events$event) {

  ssclick_position_function <- function(by_wiki = FALSE, ...) {
    events %>%
      keep_where(event == "ssclick") %>%
      group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "event_position"))) %>%
      tally %>%
      mutate(position = paste(safe_ordinals(event_position), "result")) %>%
      bar_chart(x = "position", y = "n", x_lab = "Clicked Position", y_lab = "Number of Sister Search Clicks",
                title = paste("Number of sister search clicks by clicked position, test group", switch(by_wiki, "and wiki", NULL)))
  }
  p <- ssclick_position_function() + wmf::theme_min()
  ggsave("ssclick_position_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
  rm(p)

  if (n_wiki > 1) {
    p <- ssclick_position_function(by_wiki = TRUE) +
      facet_wrap(~ wiki, nrow = n_wiki, scales = "free") +
      wmf::theme_facet()
    ggsave("ssclick_position_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 5 * n_wiki, width = fig_width)
    rm(p)
  }

}
