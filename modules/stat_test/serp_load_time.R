serp_loadtime_function <- function(by_wiki = FALSE, ...) {
  events %>%
    keep_where(event == "searchResultPage", `some same-wiki results` == TRUE) %>%
    ggplot(aes(x = load_time)) +
    scale_x_log10() +
    geom_density(aes(group = group, colour = group, fill = group), alpha = 0.3) +
    scale_color_brewer("Group", palette = "Set1") +
    scale_fill_brewer("Group", palette = "Set1") +
    labs(x = "Load Time (ms)", y = "Density", title = paste("Distribution of search result page load time by test group", switch(by_wiki, "and wiki", NULL)))
}
p <- serp_loadtime_function() + wmf::theme_min()
ggsave("serp_loadtime_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
rm(p)

if (n_wiki > 1) {
  p <- serp_loadtime_function(by_wiki = TRUE) +
    facet_wrap(~ wiki, ncol = 3, scales = "free") +
    wmf::theme_facet()
  ggsave("serp_loadtime_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 3 * ceiling(n_wiki / 3)), width = fig_width, limitsize = FALSE)
  rm(p)
}
