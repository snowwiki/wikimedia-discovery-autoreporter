scroll_serp_function <- function(by_wiki = FALSE, ...) {
  events %>%
    keep_where(event == "searchResultPage", `some same-wiki results` == TRUE) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group"))) %>%
    summarize(scrolls = sum(n_scroll_serp > 0), n_serp = n()) %>%
    ungroup %>%
    cbind(
      as.data.frame(binom:::binom.bayes(x = .$scrolls, n = .$n_serp, conf.level = 0.95, tol = 1e-9))
    ) %>%
    pointrange_chart(y_lab = "Proportion of SERPs",
                     title = paste("Proportion of search results pages with scroll by test group", switch(by_wiki, "and wiki", NULL)),
                     subtitle = "With 95% credible intervals.")
}
p <- scroll_serp_function() + wmf::theme_facet(border = FALSE)
ggsave("scroll_serp_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
rm(p)

if (n_wiki > 1) {
  p <- scroll_serp_function(by_wiki = TRUE) +
    facet_wrap(~ wiki, ncol = 3, scales = "free_y") +
    wmf::theme_facet()
  ggsave("scroll_serp_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 4 * ceiling(n_wiki / 3)), width = fig_width)
  rm(p)
}

if (exists("fulltext_from_auto")) {

  temp <- events %>% keep_where(event == "searchResultPage", from_autocomplete == TRUE, `some same-wiki results` == TRUE)
  temp$SurvObj <- with(temp, survival::Surv(max_checkin_serp, status == 2))
  fit <- survival::survfit(SurvObj ~ group, data = temp)
  ggsurv <- survminer::ggsurvplot(
    fit,
    conf.int = TRUE,
    xlab = "T (Dwell Time in seconds)",
    ylab = "Proportion of SERPs longer than T (P%)",
    surv.scale = "percent",
    palette = "Set1",
    legend = "bottom",
    legend.title = "Group",
    legend.labs = params$test_group_names,
    ggtheme = wmf::theme_min()
  )
  p <- ggsurv$plot +
    labs(
      title = "Proportion of search results pages from autocomplete last longer than T, by test group",
      subtitle = "With 95% confidence intervals."
    )
  ggsave("serp_survival_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
  rm(temp, p)

  if (n_wiki > 1) {
    # TODO: duplicated code serp_survival_all
    temp <- events %>% keep_where(event == "searchResultPage", from_autocomplete == TRUE, `some same-wiki results` == TRUE)
    temp$SurvObj <- with(temp, survival::Surv(max_checkin_serp, status == 2))
    fit <- survival::survfit(SurvObj ~ group + wiki, data = temp)
    ggsurv <- survminer::ggsurvplot(
      fit,
      conf.int = TRUE,
      xlab = "T (Dwell Time in seconds)",
      ylab = "Proportion of SERPs longer than T (P%)",
      surv.scale = "percent",
      palette = colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_wiki*length(report_params$test_group_names)),
      legend = "bottom",
      legend.title = "Group",
      ggtheme = wmf::theme_facet()
    )
    p <- ggsurv$plot +
      facet_wrap(~ wiki, ncol = 1, scales = "free_y") +
      labs(
        title = "Proportion of search results pages from autocomplete last longer than T, by test group and wiki",
        subtitle = "With 95% confidence intervals."
      )
    ggsave("serp_survival_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 4 * n_wiki, width = fig_width)
    rm(temp, p)
  }

}
