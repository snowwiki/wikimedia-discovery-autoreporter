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
ggsave("scroll_serp_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
rm(p)

if (n_wiki > 1) {
  p <- scroll_serp_function(by_wiki = TRUE) +
    facet_wrap(~ wiki, ncol = 3, scales = "free_y") +
    wmf::theme_facet()
  ggsave("scroll_serp_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 3 * ceiling(n_wiki / 3)), width = fig_width, limitsize = FALSE)
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
    color = "group",
    palette = "Set1",
    legend = "bottom",
    legend.title = "Group",
    legend.labs = traditional_test_groups,
    ggtheme = wmf::theme_min()
  )
  p <- ggsurv$plot +
    labs(
      title = "Proportion of search results pages from autocomplete last longer than T, by test group",
      subtitle = "With 95% confidence intervals."
    )
  ggsave("serp_survival_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
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
      color = "group",
      palette = "Set1",
      legend = "bottom",
      legend.title = "Group",
      ggtheme = wmf::theme_facet()
    )
    p <- ggsurv$plot +
      facet_wrap(~ wiki, ncol = 3, scales = "free_y") +
      labs(
        title = "Proportion of search results pages from autocomplete last longer than T, by test group and wiki",
        subtitle = "With 95% confidence intervals."
      )
    ggsave("serp_survival_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki < 4, fig_height, 3 * ceiling(n_wiki / 3)), width = fig_width, limitsize = FALSE)
    rm(temp, p)
  }

}
