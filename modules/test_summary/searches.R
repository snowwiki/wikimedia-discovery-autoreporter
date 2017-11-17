searches_summary_table <- searches %>%
  group_by(`Test group` = group, wiki) %>%
  summarize(
    `Search sessions` = length(unique(session_id)),
    `Searches recorded` = n()
  )

p <- searches %>%
  group_by(group, wiki, date) %>%
  summarize(`All Searches` = n(), `Searches with Results` = sum(`got same-wiki results`), `Searches with Clicks` = sum(`same-wiki clickthrough`)) %>%
  tidyr::gather(key = Type, value = count, -date, -group, -wiki) %>%
  ggplot(aes(x = date, y = count, colour = Type)) +
  geom_line(size = 1.2) +
  scale_x_date(name = "Date") +
  scale_y_continuous(labels = polloi::compress, name = "Number of Searches") +
  ggtitle("Number of all searches, searches with results and searches with clickthrough") +
  facet_grid(wiki ~ group, scales = "free_y") +
  wmf::theme_facet()
ggsave("daily_searches.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = ifelse(n_wiki > 2, 2 * n_wiki, fig_height), width = fig_width, limitsize = FALSE)
rm(p)

# Searches with n same-wiki results returned
n_results_summary_function <- function(by_wiki = FALSE, ...) {
  events %>%
    keep_where(event == "searchResultPage") %>%
    mutate(results = if_else(n_results >= 5, "5+ results", Pluralize(n_results, "result"))) %>%
    group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "results"))) %>%
    summarize(searches = length(unique(search_id[!is.na(search_id)]))) %>%
    bar_chart(x = "results", y = "searches", x_lab = "Number of same-wiki results returned",
              y_lab = "Number of searches", title = paste("Number of searches with n same-wiki result returned, by test group", switch(by_wiki, "and wiki", NULL)))
}
p <- n_results_summary_function() + wmf::theme_min()
ggsave("n_results_summary_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
rm(p)

if (n_wiki > 1) {
  p <- n_results_summary_function(by_wiki = TRUE) +
    wmf::theme_facet() +
    ggplot2::facet_wrap(~ wiki, nrow = n_wiki, scales = "free")
  ggsave("n_results_summary_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 4 * n_wiki, width = fig_width, limitsize = FALSE)
  rm(p)
}

# SERPs by offset
if (exists("serp_offset")) {
  serp_offset_summary_function <- function(by_wiki = FALSE, ...) {
    serp_offset %>%
      keep_where(!is.na(offset)) %>%
      mutate(offset = case_when(
        offset == 0 ~ "No offset (page 1)",
        offset >= 100 ~ "100+ results",
        TRUE ~ Pluralize(offset, "result")
      )) %>%
      dplyr::left_join(events, by = c("session_id", "event_id", "search_id")) %>%
      group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "offset"))) %>%
      tally %>%
      bar_chart(x = "offset", y = "n", x_lab = "Offset", y_lab = "Number of SERPs",
                title = paste("Number of SERPs with n offset results, by test group", switch(by_wiki, "and wiki", NULL)),
                caption = "This can be regarded as a proxy for users visiting additional pages of their search results.") +
      scale_x_discrete(limits = c("No offset (page 1)", Pluralize(c(20, 40, 60, 80), "result"), "100+ results")) # will remove offsets not in (0, 20, 40, 80, 100+), warning message would be generated
  }
  p <- serp_offset_summary_function() + wmf::theme_min()
  ggsave("serp_offset_summary_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width, limitsize = FALSE)
  rm(p)

  if (n_wiki > 1) {
    p <- serp_offset_summary_function(by_wiki = TRUE) +
      wmf::theme_facet() +
      ggplot2::facet_wrap(~ wiki, nrow = n_wiki, scales = "free")
    ggsave("serp_offset_summary_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 4 * n_wiki, width = fig_width, limitsize = FALSE)
    rm(p)
  }
}
