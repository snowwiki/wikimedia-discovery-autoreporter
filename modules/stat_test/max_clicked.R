max_clicked_function <- function(by_wiki = FALSE, ...) {
  searches %>%
    keep_where(
      `got same-wiki results`,
      `same-wiki clickthrough`,
      !is.na(`max clicked position (same-wiki)`)
    ) %>%
    mutate(
      `max clicked position (same-wiki)` = case_when(
        `max clicked position (same-wiki)` < 4 ~ safe_ordinals(`max clicked position (same-wiki)` + 1),
        `max clicked position (same-wiki)` >= 4 & `max clicked position (same-wiki)` < 20 ~ "5th - 20th",
        TRUE ~ "21st or higher"
      )
    ) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "max clicked position (same-wiki)"))) %>%
    summarize(counts = n()) %>%
    mutate(total = sum(counts)) %>%
    ungroup %>%
    cbind(
      as.data.frame(binom:::binom.bayes(.$counts, n = .$total, conf.level = 0.95, tol = 1e-9))
    ) %>%
    mutate(`max clicked position (same-wiki)` = factor(`max clicked position (same-wiki)`, levels = c("1st", "2nd", "3rd", "4th", "5th - 20th", "21st or higher"))) %>%
    pointrange_chart(
      y_lab = "Proportion of searches",
      title = paste("Maximum clicked position for same-wiki results, by test group", switch(by_wiki, "and wiki", NULL)),
      subtitle = "With 95% credible intervals."
    )
}

p <- max_clicked_function() +
  facet_wrap(~ `max clicked position (same-wiki)`, ncol = 6, scales = "free_y") +
  wmf::theme_facet(axis.text.x = element_text(angle = 90))
ggsave("max_clicked_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
rm(p)

if (n_wiki > 1) {
  p <- max_clicked_function(by_wiki = TRUE) +
    facet_wrap(~ wiki + `max clicked position (same-wiki)`, ncol = 6, scales = "free_y") +
    wmf::theme_facet(axis.text.x = element_text(angle = 90))
  ggsave("max_clicked_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 4 * n_wiki, width = 12)
  rm(p)
}
