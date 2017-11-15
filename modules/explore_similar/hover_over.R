if (exists("hover_over")) {

  hover_summary <- hover_over %>%
    keep_where(event == "hover-on") %>%
    mutate(results = ifelse(results >= 5, "5+ results", Pluralize(results, "result"))) %>%
    group_by(section, results) %>%
    tally %>%
    xtabs(n ~ section + results, data = .) %>%
    addmargins

  hover_breakdown_function <- function(by_wiki = FALSE, ...) {
    hover_over %>%
      keep_where(event == "hover-on") %>%
      mutate(
        section = ifelse(is.na(section), "NA", section),
        results = ifelse(is.na(results), "NA", ifelse(results >= 5, "5+ results", Pluralize(results, "result")))
      ) %>%
      group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "section", "results"))) %>%
      tally %>%
      bar_chart(x = "section", y = "n", x_lab = "Section", y_lab = "Number of Hover-Over", geom_text_size = 2.5,
                title = paste("Number of hover-over events by section, number of results, test group", switch(by_wiki, "and wiki", NULL)))
  }
  p <- hover_breakdown_function() +
    facet_wrap(~ results, scales = "free_y") +
    wmf::theme_min(axis.text.x = element_text(angle = 90))
  ggsave("hover_breakdown_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 8, width = fig_width)
  rm(p)

  if (n_wiki > 1) {
    p <- hover_breakdown_function(by_wiki = TRUE) +
      facet_grid(wiki ~ results, scales = "free_y") +
      wmf::theme_facet(axis.text.x = element_text(angle = 90))
    ggsave("hover_breakdown_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 5 * n_wiki, width = 12)
    rm(p)
  }

}

if (all(c("hover-on", "hover-off") %in% events$event) & !is.null(events$event_extraParams)) {

  hover_rate <- searches %>%
    keep_where(`got same-wiki results`) %>%
    group_by(group, wiki) %>%
    summarize(hover = sum(n_hover > 0), n_search = n()) %>%
    ungroup %>%
    cbind(
      as.data.frame(binom:::binom.bayes(x = .$hover, n = .$n_search, conf.level = 0.95, tol = 1e-9))
    )

  p <- searches %>%
    keep_where(`got same-wiki results`) %>%
    group_by(date, group, wiki) %>%
    summarize(hover = sum(n_hover > 0), n_search = n()) %>%
    ungroup %>%
    cbind(
      as.data.frame(binom:::binom.bayes(x = .$hover, n = .$n_search, conf.level = 0.95, tol = 1e-9))
    ) %>%
    ggplot(aes(x = date, color = group, y = mean, ymin = lower, ymax = upper)) +
    geom_hline(data = hover_rate, aes(yintercept = mean, color = group), linetype = "dashed") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.1, color = NA) +
    geom_line() +
    scale_color_brewer("Group", palette = "Set1") +
    scale_fill_brewer("Group", palette = "Set1") +
    scale_y_continuous("Hover-over Rate", labels = scales::percent_format()) +
    facet_wrap(~ wiki, ncol = 2, scales = "free_y") +
    labs(title = "Daily search-wise hover-over rates", subtitle = "Dashed lines mark the overall hover-over rate") +
    wmf::theme_facet()
  ggsave("daily_hover_rate.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 3*ceiling(n_wiki/2), width = fig_width)
  rm(p)

  rm(hover_rate)

}
