first_clicked_function <- function(by_wiki = FALSE, ...) {
  searches %>%
    keep_where(
      `got same-wiki results`,
      `same-wiki clickthrough`,
      !is.na(`first clicked same-wiki results position`)
    ) %>%
    mutate(`first clicked result's position` = ifelse(`first clicked same-wiki results position` < 4, safe_ordinals(`first clicked same-wiki results position` + 1), "5th or higher")) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "first clicked result's position"))) %>%
    tally %>%
    mutate(total = sum(n)) %>%
    ungroup %>%
    cbind(
      as.data.frame(binom:::binom.bayes(.$n, n = .$total, conf.level = 0.95, tol = 1e-9))
    ) %>%
    pointrange_chart(
      y_lab = "Proportion of searches",
      title = paste("Position of the first clicked same-wiki result by test group", switch(by_wiki, "and wiki", NULL)),
      subtitle = "With 95% credible intervals."
    )
}

p <- first_clicked_function() +
  facet_wrap(~ `first clicked result's position`, ncol = 5, scales = "free_y") +
  wmf::theme_facet(axis.text.x = element_text(angle = 90))
ggsave("first_clicked_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
rm(p)

if (n_wiki > 1) {
  p <- first_clicked_function(by_wiki = TRUE) +
    facet_wrap(~ wiki + `first clicked result's position`, ncol = 5, scales = "free_y") +
    wmf::theme_facet(axis.text.x = element_text(angle = 90))
  ggsave("first_clicked_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 4 * n_wiki, width = 12)
  rm(p)
}
