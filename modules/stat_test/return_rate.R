return_rate_function <- function(by_wiki = FALSE, ...) {
  returnRate_to_same_search <- events %>%
    keep_where(!(event %in% c("visitPage", "checkin"))) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "serp_id"))) %>%
    keep_where(sum(grepl("click", event)) > 0) %>% # Among search with at least 1 click
    arrange(group, serp_id, timestamp) %>%
    mutate(n_click_cumsum = cumsum(grepl("click", event))) %>%
    keep_where(n_click_cumsum > 0) %>% # delete serp and hover before first click
    summarize(comeback = sum(event %in% c("searchResultPage", "hover-on", "hover-off")) > 0 | sum(n_click_cumsum > 1) > 0) %>% # comeback to the same serp or make another click or hover
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group"))) %>%
    summarize(return_to_same_search = sum(comeback), n_search = length(unique(serp_id))) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group"))) %>%
    dplyr::do(binom::binom.bayes(.$return_to_same_search, .$n_search, conf.level = 0.95, tol = 1e-9))

  returnRate_to_other_search <- events %>%
    keep_where(!(event %in% c("visitPage", "checkin"))) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "session_id"))) %>%
    keep_where(sum(grepl("click", event)) > 0) %>% # Among session with at least 1 click
    arrange(group, session_id, timestamp) %>%
    mutate(n_click_cumsum = cumsum(grepl("click", event))) %>%
    keep_where(n_click_cumsum > 0) %>% # delete serp before first click
    summarize(another_search = length(unique(serp_id)) > 1) %>% # comeback to make another search
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group"))) %>%
    summarize(return_to_make_another_search = sum(another_search), n_session = length(unique(session_id))) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group"))) %>%
    dplyr::do(binom::binom.bayes(.$return_to_make_another_search, .$n_session, conf.level = 0.95, tol = 1e-9))

  dplyr::bind_rows("Return to the same search page" = returnRate_to_same_search,
                   "Return to make different search" = returnRate_to_other_search, .id = "type") %>%
    pointrange_chart(y_lab = "Return rate", title = paste("Return rate after users clickthrough on search engine result pages", switch(by_wiki, "by wiki", NULL)),
                     subtitle = "With 95% credible intervals.")
}
p <- return_rate_function() +
  facet_wrap(~ type, scales = "free_y") +
  wmf::theme_facet(border = FALSE)
ggsave("return_rate_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
rm(p)

if (n_wiki > 1) {
  p <- return_rate_function(by_wiki = TRUE) +
    facet_grid(wiki ~ type, scales = "free_y") +
    wmf::theme_facet()
  ggsave("return_rate_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 4 * n_wiki, width = fig_width)
  rm(p)
}
