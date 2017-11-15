if (exists("serp_offset")) {
  search_offset_function <- function(by_wiki = FALSE, ...) {
    serp_offset %>%
      group_by(session_id, serp_id) %>%
      summarize(`Any page-turning` = any(offset > 0)) %>%
      dplyr::right_join(searches, by = c("session_id", "serp_id")) %>%
      group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group"))) %>%
      summarize(page_turn = sum(`Any page-turning`, na.rm = TRUE), n_search = n()) %>%
      ungroup %>%
      cbind(
        as.data.frame(binom:::binom.bayes(x = .$page_turn, n = .$n_search, conf.level = 0.95, tol = 1e-9))
      ) %>%
      pointrange_chart(
        y_lab = "Proportion of searches",
        title = paste("Proportion of searches with clicks to see other pages of the search results, by test group", switch(by_wiki, "and wiki", NULL)),
        subtitle = "With 95% credible intervals."
      )
  }

  p <- search_offset_function() + wmf::theme_facet(border = FALSE)
  ggsave("search_offset_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
  rm(p)

  if (n_wiki > 1) {
    p <- search_offset_function(by_wiki = TRUE) +
      facet_wrap(~ wiki, ncol = 3, scales = "free_y") +
      wmf::theme_facet()
    ggsave("search_offset_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 4 * ceiling(n_wiki / 3)), width = fig_width)
    rm(p)
  }

}
