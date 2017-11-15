p <- searches %>%
  ungroup %>%
  keep_where(`same-wiki clickthrough` == TRUE) %>%
  select(c(group, `Query score (F=0.1)`, `Query score (F=0.5)`, `Query score (F=0.9)`)) %>%
  tidyr::gather(`F value`, `Query score`, -group) %>%
  mutate(`F value` = sub("^Query score \\(F=(0\\.[159])\\)$", "F = \\1", `F value`)) %>%
  group_by(group, `F value`) %>%
  summarize(
    PaulScore = mean(`Query score`),
    Interval = paste0(quantile(bootstrap_mean(`Query score`, 1000), c(0.025, 0.975)), collapse = ",")
  ) %>%
  tidyr::extract(Interval, into = c("Lower", "Upper"), regex = "(.*),(.*)", convert = TRUE) %>%
  ggplot(aes(x = `F value`, y = PaulScore, color = group)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.7)) +
  scale_color_brewer("Group", palette = "Set1") +
  labs(
    x = NULL, y = "PaulScore",
    title = "PaulScore by test group and value of F",
    subtitle = "With bootstrapped 95% confidence intervals."
  ) +
  geom_text(aes(label = sprintf("%.3f", PaulScore), y = Upper + 0.01, vjust = "bottom"),
            position = position_dodge(width = 0.7)) +
  wmf::theme_min()
ggsave("paulscores_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
rm(p)

if (n_wiki > 1) {
  # TODO: duplicated code paulscores_all
  p <- searches %>%
    ungroup %>%
    keep_where(`same-wiki clickthrough` == TRUE) %>%
    select(c(wiki, group, `Query score (F=0.1)`, `Query score (F=0.5)`, `Query score (F=0.9)`)) %>%
    tidyr::gather(`F value`, `Query score`, -c(group, wiki)) %>%
    mutate(`F value` = sub("^Query score \\(F=(0\\.[159])\\)$", "F = \\1", `F value`)) %>%
    group_by(wiki, group, `F value`) %>%
    summarize(
      PaulScore = mean(`Query score`),
      Interval = paste0(quantile(bootstrap_mean(`Query score`, 1000), c(0.025, 0.975)), collapse = ",")
    ) %>%
    tidyr::extract(Interval, into = c("Lower", "Upper"), regex = "(.*),(.*)", convert = TRUE) %>%
    ggplot(aes(x = `F value`, y = PaulScore, color = group)) +
    geom_pointrange(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.7)) +
    scale_color_brewer("Group", palette = "Set1") +
    labs(
      x = NULL, y = "PaulScore",
      title = "PaulScore by wiki, test group and value of F",
      subtitle = "With bootstrapped 95% confidence intervals."
    ) +
    geom_text(aes(label = sprintf("%.3f", PaulScore), y = Upper + 0.01, vjust = "bottom"),
              position = position_dodge(width = 0.7)) +
    facet_wrap(~ wiki, ncol = 3, scales = "free_y") +
    wmf::theme_facet()
  ggsave("paulscores_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 4 * ceiling(n_wiki / 3)), width = fig_width)
  rm(p)
}
