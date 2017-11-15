if (exists("esclick_result")) {

  esclick_summary <- esclick_result %>%
    mutate(
      section = ifelse(is.na(section), "NA", section),
      position = ifelse(is.na(position), "NA", paste(safe_ordinals(position + 1), "result"))
    ) %>%
    group_by(section, position) %>%
    tally %>%
    xtabs(n ~ section + position, data = .) %>%
    addmargins

  esclick_breakdown_function <- function(by_wiki = FALSE, ...) {
    esclick_result %>%
      mutate(
        section = ifelse(is.na(section), "NA", section),
        position = ifelse(is.na(position), "NA", paste(safe_ordinals(position + 1), "result"))
      ) %>%
      group_by(!!! rlang::syms(c(switch(by_wiki, "wiki", NULL), "group", "section", "position"))) %>%
      tally %>%
      bar_chart(x = "section", y = "n", x_lab = "Section", y_lab = "Number of Explore Similar Clicks",
                title = paste("Number of explore similar clicks by section, clicked position, test group", switch(by_wiki, "and wiki", NULL)))
  }
  p <- esclick_breakdown_function() +
    facet_wrap(~ position, scales = "free_y") +
    wmf::theme_min(axis.text.x = element_text(angle = 90))
  ggsave("esclick_breakdown_all.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = fig_height, width = fig_width)
  rm(p)

  if (n_wiki > 1) {
    p <- esclick_breakdown_function(by_wiki = TRUE) +
      facet_grid(wiki ~ position, scales = "free_y") +
      wmf::theme_facet(axis.text.x = element_text(angle = 90))
    ggsave("esclick_breakdown_wiki.png", p, path = fig_path, units = "in", dpi = plot_resolution, height = 5 * n_wiki, width = fig_width)
    rm(p)
  }

}
