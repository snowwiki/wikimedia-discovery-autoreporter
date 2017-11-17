samewiki_ctr_function <- function(by_wiki = FALSE, ...) {
  searches %>%
    keep_where(`got same-wiki results` == TRUE) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group"))) %>%
    summarize(clickthroughs = sum(`same-wiki clickthrough`), n_search = n()) %>%
    ungroup %>%
    cbind(
      as.data.frame(binom:::binom.bayes(x = .$clickthroughs, n = .$n_search, conf.level = 0.95, tol = 1e-9))
    )
}
samewiki_ctr_all <- samewiki_ctr_function() # save object samewiki_ctr_all for odds ratio plot
p <- samewiki_ctr_all %>%
  pointrange_chart(y_lab = "Clickthrough rate", title = "Same-wiki clickthrough rates by test group",
                   subtitle = "With 95% credible intervals.") +
  wmf::theme_facet(border = FALSE)
ggsave("engagement_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
rm(p)

if (n_wiki > 1) {
  samewiki_ctr_bywiki <- samewiki_ctr_function(by_wiki = TRUE) # save object samewiki_ctr_bywiki for odds ratio plot
  p <- samewiki_ctr_bywiki %>%
    pointrange_chart(y_lab = "Clickthrough rate", title = "Same-wiki clickthrough rates by test group and wiki",
                     subtitle = "With 95% credible intervals.") +
    facet_wrap(~ wiki, ncol = 3, scales = "free_y") +
    wmf::theme_facet()
  ggsave("engagement_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 3 * ceiling(n_wiki / 3)), width = fig_width, limitsize = FALSE)
  rm(p)
}

# Engagement odds ratio for all
control_group <- grep("control", traditional_test_groups, value = TRUE)
test_group <- setdiff(traditional_test_groups, control_group)

for (this_group in test_group) {
  this_plot <- samewiki_ctr_all %>%
    keep_where(group %in% c(control_group, this_group)) %>%
    arrange(desc(group)) %>%
    dplyr::do(BCDA::tidy(BCDA::beta_binom(.$x, .$n), interval_type = "HPD")) %>%
    ungroup %>%
    mutate(term = factor(
      term,
      levels = c("p2", "p1", "prop_diff", "relative_risk", "odds_ratio"),
      labels = c("Pr[Control Engaging]", "Pr[Test Engaging]", "Pr[Test] - Pr[Control]", "Relative Risk", "Odds Ratio")
    )) %>%
    ggplot(aes(x = 1, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_text(aes(label = round(estimate, 4), y = estimate, hjust = "left"), nudge_x = 0.005) +
    facet_grid(term ~ ., scales = "free_y") +
    scale_x_continuous(limits = c(0.99, 1.02)) +
    labs(
      x = NULL, y = "Estimate",
      title = "How likely test group users were to engage with search results",
      subtitle = "95% credible intervals calculated as Highest Posterior Density (HPD) intervals"
    ) +
    wmf::theme_facet(strip.text.y = element_text(size = 12, angle = 0), clean_xaxis = TRUE)
  ggsave(paste0(this_group, "_engagement_OR_all.png"), this_plot, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
  rm(this_plot)
}

# Engagement odds ratio by wiki
if (n_wiki > 1) {
  # TODO: duplicated code engagement_OR_all
  for (this_group in test_group) {
    this_plot <- samewiki_ctr_bywiki %>%
      keep_where(group %in% c(control_group, this_group)) %>%
      arrange(wiki, desc(group)) %>%
      group_by(wiki) %>%
      dplyr::do(BCDA::tidy(BCDA::beta_binom(.$x, .$n), interval_type = "HPD")) %>%
      ungroup %>%
      mutate(term = factor(
        term,
        levels = c("p2", "p1", "prop_diff", "relative_risk", "odds_ratio"),
        labels = c("Pr[Control Engaging]", "Pr[Test Engaging]", "Pr[Test] - Pr[Control]", "Relative Risk", "Odds Ratio")
      )) %>%
      ggplot(aes(x = 1, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_linerange() +
      geom_label(aes(label = round(estimate, 3)), show.legend = FALSE) +
      facet_grid(term ~ wiki, scales = "free_y") +
      scale_x_continuous(limits = c(0.99, 1.01)) +
      labs(
        x = NULL, y = "Estimate",
        title = "How likely test group users were to engage with search results",
        subtitle = "95% credible intervals calculated as Highest Posterior Density (HPD) intervals"
      ) +
      wmf::theme_facet(strip.text.y = element_text(size = 12), clean_xaxis = TRUE)
    ggsave(paste0(this_group, "_engagement_OR_wiki.png"), this_plot, path = fig_path, units = "in", dpi = plot_resolution, height = 10, width = 12, limitsize = FALSE)
    rm(this_plot)
  }
}

# Daily CTR
ctr <- searches %>%
  keep_where(`got same-wiki results` == TRUE) %>%
  group_by(group, wiki) %>%
  summarize(n_search = n(), clickthroughs = sum(`same-wiki clickthrough`)) %>%
  ungroup %>%
  cbind(
    as.data.frame(binom:::binom.bayes(x = .$clickthroughs, n = .$n_search, conf.level = 0.95, tol = 1e-9))
  )

p <- searches %>%
  keep_where(`got same-wiki results` == TRUE) %>%
  group_by(date, group, wiki) %>%
  summarize(n_search = n(), clickthroughs = sum(`same-wiki clickthrough`)) %>%
  ungroup %>%
  cbind(
    as.data.frame(binom:::binom.bayes(x = .$clickthroughs, n = .$n_search, conf.level = 0.95, tol = 1e-9))
  ) %>%
  ggplot(aes(x = date, color = group, y = mean, ymin = lower, ymax = upper)) +
  geom_hline(data = ctr, aes(yintercept = mean, color = group), linetype = "dashed") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.1, color = NA) +
  geom_line() +
  scale_color_brewer("Group", palette = "Set1") +
  scale_fill_brewer("Group", palette = "Set1") +
  scale_y_continuous("Clickthrough Rate", labels = scales::percent_format()) +
  facet_wrap(~ wiki, ncol = 2, scales = "free_y") +
  labs(title = "Daily search-wise clickthrough rates rate by group", subtitle = "Dashed lines mark the overall clickthrough rate") +
  wmf::theme_facet()
ggsave("daily_ctr.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 3, fig_height, 3 * ceiling(n_wiki / 2)), width = fig_width, limitsize = FALSE)
rm(p)
rm(ctr)
