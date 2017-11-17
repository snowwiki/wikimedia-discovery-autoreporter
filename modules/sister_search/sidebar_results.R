if (exists("serp_iw")){

  ss_results_summary <- serp_iw %>%
    keep_where(!is.na(source), !is.na(position)) %>%
    mutate(position = paste(safe_ordinals(position), "result")) %>%
    group_by(source, position) %>%
    summarize(counts = length(unique(event_id))) %>%
    xtabs(counts ~ source + position, data = .) %>%
    addmargins

  serp_iw_breakdown_function <- function(by_wiki = FALSE, ...) {
    serp_iw %>%
      keep_where(!is.na(source), !is.na(position)) %>%
      dplyr::left_join(events, by = c("session_id", "event_id", "search_id")) %>%
      mutate(position = safe_ordinals(position)) %>%
      group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "source", "position"))) %>%
      summarize(counts = length(unique(event_id))) %>%
      bar_chart(x = "position", y = "counts", x_lab = "Position", y_lab = "Number of SERPs", geom_text_size = 2,
                title = paste("Number of SERPs' sister-search sidebar results by source, position, test group", switch(by_wiki, "and wiki", NULL)))
  }
  p <- serp_iw_breakdown_function() +
    facet_wrap(~ source, ncol = 2, scales = "free_y") +
    wmf::theme_min()
  ggsave("serp_iw_breakdown_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 12, width = fig_width, limitsize = FALSE)
  rm(p)

  if (n_wiki > 1){
    p <- serp_iw_breakdown_function(by_wiki = TRUE) +
      facet_grid(wiki ~ source, scales = "free") +
      wmf::theme_facet()
    ggsave("serp_iw_breakdown_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 3 * n_wiki, width = 12, limitsize = FALSE)
    rm(p)
  }

  p <- serp_iw %>%
    keep_where(!is.na(source) | !is.na(position)) %>%
    dplyr::left_join(events, by = c("session_id", "event_id", "search_id")) %>%
    group_by(group, wiki) %>%
    summarize(counts = length(unique(event_id))) %>%
    bar_chart(x = "wiki", y = "counts", x_lab = "Wiki", y_lab = "Number of SERPs",
              title = "Number of SERPs with sidebar results, by test group and wiki") +
    wmf::theme_min(axis.text.x = element_text(angle = 90))
  ggsave("serp_iw_na.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = 12, limitsize = FALSE)
  rm(p)

}
