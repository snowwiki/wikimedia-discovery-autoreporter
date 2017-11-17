event_count_function <- function(by_wiki = FALSE, ...) {
  events %>%
    keep_where(!(event %in% c("visitPage", "checkin"))) %>%
    mutate(event = factor(event, levels = c("searchResultPage", "click", "ssclick", "iwclick", "hover-on", "esclick", "hover-off"))) %>% # Order the bars
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "event"))) %>%
    tally %>%
    bar_chart(x = "event", y = "n", x_lab = "Event type", y_lab = "Number of events", title = paste("Number of events on search result pages by test group", switch(by_wiki, "and wiki", NULL)))
}
p <- event_count_function() + wmf::theme_min()
ggsave("event_count_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
rm(p)

event_after_click_function <- function(by_wiki = FALSE, ...) {
  events %>%
    keep_where(event %in% c("visitPage", "checkin")) %>%
    mutate(event = factor(event, levels = c("visitPage", "checkin"))) %>% # Order the bars
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "event"))) %>%
    tally %>%
    bar_chart(x = "event", y = "n", x_lab = "Event type", y_lab = "Number of events",
              title = paste("Number of events on articles after clickthrough, by test group", switch(by_wiki, "and wiki", NULL)),
              subtitle = "Only the last check-in events are kept.")
}
p <- event_after_click_function() + wmf::theme_min()
ggsave("event_after_click_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
rm(p)

if (n_wiki > 1) {
  p <- event_count_function(by_wiki = TRUE) +
    wmf::theme_facet() +
    facet_wrap(~ wiki, ncol = 2, scales = "free")
  ggsave("event_count_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 3, fig_height, 3 * ceiling(n_wiki / 2)), width = fig_width, limitsize = FALSE)
  rm(p)

  p <- event_after_click_function(by_wiki = TRUE) +
    wmf::theme_facet() +
    facet_wrap(~ wiki, ncol = 2, scales = "free")
  ggsave("event_after_click_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 3, fig_height, 3 * ceiling(n_wiki / 2)), width = fig_width, limitsize = FALSE)
  rm(p)
}
