if (!is.null(report_params$interleaved_group_names)) {

  temp <- dplyr::inner_join(
    visitedPages[visitedPages$group %in% report_params$interleaved_group_names, ],
    events_interleaved[
      !is.na(team) & team != "" & event %in% c("visitPage", "checkin") & valid == TRUE,
      c("wiki", "group", "session_id", "page_id", "team"),
      with = TRUE
      ],
    by = c("wiki", "group", "session_id", "page_id")
    )
  temp$SurvObj <- with(temp, survival::Surv(dwell_time, status == 2))

  fit <- survival::survfit(SurvObj ~ team + group, data = temp)
  ggsurv <- survminer::ggsurvplot(
    fit,
    conf.int = TRUE,
    xlab = "T (Dwell Time in seconds)",
    ylab = "Proportion of visits longer than T (P%)",
    surv.scale = "percent",
    palette = colorRampPalette(RColorBrewer::brewer.pal(9, "Dark2"))(2 * length(report_params$interleaved_group_names)),
    legend = "bottom",
    legend.title = "",
    ggtheme = wmf::theme_facet()
  )
  p <- ggsurv$plot +
    facet_wrap(~ group, scales = "free_y") +
    labs(
      title = "How long users stay on each team's results",
      subtitle = "With 95% confidence intervals."
    )
  ggsave("interleaved_survival_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
  rm(p)

  if (n_wiki > 1) {

    for (this_group in report_params$interleaved_group_names) {
      fit <- survival::survfit(SurvObj ~ team + wiki, data = temp[temp$group == this_group, ])
      ggsurv <- survminer::ggsurvplot(
        fit,
        conf.int = TRUE,
        xlab = "T (Dwell Time in seconds)",
        ylab = "Proportion of visits longer than T (P%)",
        surv.scale = "percent",
        palette = colorRampPalette(RColorBrewer::brewer.pal(9, "Dark2"))(2 * n_wiki),
        legend = "bottom",
        legend.title = "",
        ggtheme = wmf::theme_facet()
      )
      p <- ggsurv$plot +
        facet_wrap(~ wiki, ncol = 3, scales = "free_y") +
        labs(
          title = paste0("How long users stay on each team's results, by wiki (Group = ", this_group, ")"),
          subtitle = "With 95% confidence intervals."
        )
      ggsave(paste0(this_group, "_interleaved_survival_wiki.png"), p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 3 * ceiling(n_wiki / 3)), width = fig_width, limitsize = FALSE)
      rm(p)
    }

  }

  rm(temp)

}
