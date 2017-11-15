search_abandon_rate_function <- function(by_wiki = FALSE, ...) {
  searches %>%
    keep_where(`got same-wiki results` == TRUE) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group"))) %>%
    summarize(abandon = n() - sum(engaged), n_search = n()) %>%
    ungroup %>%
    cbind(
      as.data.frame(binom:::binom.bayes(x = .$abandon, n = .$n_search, conf.level = 0.95, tol = 1e-9))
    ) %>%
    pointrange_chart(
      y_lab = "Search Abandon Rate",
      title = paste("Proportion of searches without any actions on the result page by test group", switch(by_wiki, "and wiki", NULL)),
      subtitle = "With 95% credible intervals.")
}
p <- search_abandon_rate_function() + wmf::theme_facet(border = FALSE)
ggsave("search_abandon_rate_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
rm(p)

if (n_wiki > 1) {
  p <- search_abandon_rate_function(by_wiki = TRUE) +
    facet_wrap(~ wiki, ncol = 3, scales = "free_y") +
    wmf::theme_facet()
  ggsave("search_abandon_rate_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 4 * ceiling(n_wiki / 3)), width = fig_width)
  rm(p)
}
