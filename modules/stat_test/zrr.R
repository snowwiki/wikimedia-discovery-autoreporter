zrr_function <- function(by_wiki = FALSE, ...) {
  searches %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group"))) %>%
    summarize(zero = sum(!`got same-wiki results`), n_search = n()) %>%
    ungroup %>%
    cbind(
      as.data.frame(binom:::binom.bayes(x = .$zero, n = .$n_search, conf.level = 0.95, tol = 1e-9))
    ) %>%
    pointrange_chart(
      y_lab = "Zero Results Rate",
      title = paste("Proportion of searches that did not yield any same-wiki results, by test group", switch(by_wiki, "and wiki", NULL)),
      subtitle = "With 95% credible intervals."
    )
}
p <- zrr_function() + wmf::theme_facet(border = FALSE)
ggsave("zrr_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
rm(p)

if (n_wiki > 1) {
  p <- zrr_function(by_wiki = TRUE) +
    facet_wrap(~ wiki, ncol = 3, scales = "free_y") +
    wmf::theme_facet()
  ggsave("zrr_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 3 * ceiling(n_wiki / 3)), width = fig_width, limitsize = FALSE)
  rm(p)
}

zrr <- searches %>%
  group_by(group, wiki) %>%
  summarize(n_search = n(), zero = sum(!`got same-wiki results`)) %>%
  ungroup %>%
  cbind(
    as.data.frame(binom:::binom.bayes(x = .$zero, n = .$n_search, conf.level = 0.95, tol = 1e-9))
  )

p <- searches %>%
  group_by(date, group, wiki) %>%
  summarize(n_search = n(), zero = sum(!`got same-wiki results`)) %>%
  ungroup %>%
  cbind(
    as.data.frame(binom:::binom.bayes(x = .$zero, n = .$n_search, conf.level = 0.95, tol = 1e-9))
  ) %>%
  ggplot(aes(x = date, color = group, y = mean, ymin = lower, ymax = upper)) +
  geom_hline(data = zrr, aes(yintercept = mean, color = group), linetype = "dashed") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.1, color = NA) +
  geom_line() +
  scale_color_brewer("Group", palette = "Set1") +
  scale_fill_brewer("Group", palette = "Set1") +
  scale_y_continuous("Zero Results Rate", labels = scales::percent_format()) +
  facet_wrap(~ wiki, ncol = 2, scales = "free_y") +
  labs(title = "Daily search-wise zero results rate by group", subtitle = "Dashed lines mark the overall zero results rate") +
  wmf::theme_facet()
ggsave("daily_zrr.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 3, fig_height, 3 * ceiling(n_wiki / 2)), width = fig_width, limitsize = FALSE)
rm(p)

rm(zrr)
