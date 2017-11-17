if (!is.null(report_params$interleaved_group_names)) {

  # Overall preference
  interleaved_pref_overall_function <- function(by_wiki = FALSE, ...) {
    interleaved_pref_overall <- rbind(
      interleaved_visitpage[, j = list(
        "observed" = wmf::interleaved_preference(paste(.SD$session_id), .SD$team),
        "upper" = wmf::interleaved_confint(paste(.SD$session_id), .SD$team)$upper,
        "lower" = wmf::interleaved_confint(paste(.SD$session_id), .SD$team)$lower,
        "method" = "Sampling Sessions"
      ), by = c(switch(by_wiki, "wiki", NULL), "group")],
      interleaved_visitpage[, j = list(
        "observed" = wmf::interleaved_preference(paste(.SD$session_id, .SD$search_id), .SD$team),
        "upper" = wmf::interleaved_confint(paste(.SD$session_id, .SD$search_id), .SD$team)$upper,
        "lower" = wmf::interleaved_confint(paste(.SD$session_id, .SD$search_id), .SD$team)$lower,
        "method" = "Sampling Searches"
      ), by = c(switch(by_wiki, "wiki", NULL), "group")]
    )
    ggplot(
      mutate(interleaved_pref_overall, method = sub("\\s", "\n", method)),
      aes(x = method, y = observed)
    ) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_linerange(aes(ymin = lower, ymax = upper)) +
      geom_label(aes(label = sprintf("%.4f", observed))) +
      labs(
        x = "Bootstrap approach", y = "B ← Preference → A",
        title = paste("Preference for results from two rankers, by group", switch(by_wiki, "and wiki", NULL)),
        caption = "95% confidence intervals were bootstrapped using two different sampling approaches"
      )
  }
  p <- interleaved_pref_overall_function() +
    facet_wrap(~ group) +
    wmf::theme_facet()
  ggsave("interleaved_pref_overall.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
  rm(p)

  # Daily preference
  interleaved_pref_daily_function <- function(by_wiki = FALSE, ...) {
    interleaved_pref_daily <- rbind(
      interleaved_visitpage[, j = list(
        "observed" = wmf::interleaved_preference(paste(.SD$session_id), .SD$team),
        "upper" = wmf::interleaved_confint(paste(.SD$session_id), .SD$team)$upper,
        "lower" = wmf::interleaved_confint(paste(.SD$session_id), .SD$team)$lower,
        "method" = "Sampling Sessions"
      ), by = c("date", switch(by_wiki, "wiki", NULL), "group")],
      interleaved_visitpage[, j = list(
        "observed" = wmf::interleaved_preference(paste(.SD$session_id, .SD$search_id), .SD$team),
        "upper" = wmf::interleaved_confint(paste(.SD$session_id, .SD$search_id), .SD$team)$upper,
        "lower" = wmf::interleaved_confint(paste(.SD$session_id, .SD$search_id), .SD$team)$lower,
        "method" = "Sampling Searches"
      ), by = c("date", switch(by_wiki, "wiki", NULL), "group")]
      ) %>%
      mutate(preferred = dplyr::if_else(observed > 0, "A", "B")) %>%
      group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "method", "preferred"))) %>%
      arrange(date) %>%
      mutate(counter = cumsum(!is.na(date))) %>%
      ungroup

    ggplot(keep_where(interleaved_pref_daily, !is.na(observed)), aes(x = date, y = observed)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
      geom_line() +
      geom_segment(
        aes(xend = date, yend = ifelse(preferred == "A", 0.275, -0.275), color = preferred),
        linetype = "dotted"
      ) +
      geom_point(aes(color = preferred)) +
      geom_text(
        aes(y = ifelse(preferred == "A", 0.3, -0.3), label = counter, color = preferred),
        show.legend = FALSE, fontface = "bold"
      ) +
      scale_color_brewer(palette = "Set1") +
      labs(
        x = "Date", y = "B ← Preference → A",
        title = paste("Preference for results from two rankers, daily by group", switch(by_wiki, "and wiki", NULL)),
        subtitle = "Showing counts of how many times users preferred one ranking over the other",
        caption = "95% confidence intervals were bootstrapped using two different sampling approaches"
      )
  }
  p <- interleaved_pref_daily_function() +
    facet_grid(group ~ method, scales = "free_y") +
    wmf::theme_facet()
  ggsave("interleaved_pref_daily.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_interleaved_groups < 2, fig_height, 3 * n_interleaved_groups), width = fig_width, limitsize = FALSE)
  rm(p)

  # By wiki
  if (n_wiki > 1) {

    p <- interleaved_pref_overall_function(by_wiki = TRUE) +
      facet_grid(wiki ~ group, scales = "free_y") +
      wmf::theme_facet()
    ggsave("interleaved_pref_overall_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 3 * n_wiki, width = fig_width, limitsize = FALSE)
    rm(p)

    p <- interleaved_pref_daily_function(by_wiki = TRUE) +
      facet_grid(wiki + group ~ method, scales = "free_y") +
      wmf::theme_facet()
    ggsave("interleaved_pref_daily_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 3 * n_interleaved_groups * n_wiki, width = fig_width, limitsize = FALSE)
    rm(p)

  }

  rm(interleaved_visitpage)
}
