if ("user_agent" %in% names(events)) {

  user_agents <- dplyr::distinct(events, wiki, session_id, group, user_agent)
  user_agents <- user_agents %>%
    cbind(., purrr::map_df(.$user_agent, ~ wmf::null2na(jsonlite::fromJSON(.x, simplifyVector = FALSE)))) %>%
    mutate(
      browser = paste(browser_family, browser_major),
      os = case_when(
        is.na(os_major) ~ os_family,
        !is.na(os_major) & !is.na(os_minor) ~ paste0(os_family, " ", os_major, ".", os_minor),
        TRUE ~ paste(os_family, os_major)
      )
    )

  summarize_uas <- function(data) {
    data %>%
      tally %>%
      mutate(prop = paste0(scales::percent_format()(n/sum(n)), " (", n, ")")) %>%
      select(-n) %>%
      tidyr::spread(group, prop) %>%
      ungroup
  }

  get_bayes_factor <- function(data) {
    BF <- data %>%
      tally %>%
      tidyr::spread(group, n) %>%
      ungroup %>%
      select(dplyr::one_of(report_params$test_group_names)) %>%
      as.matrix() %>%
      # see http://bayesfactorpcl.r-forge.r-project.org/#ctables for more info
      BayesFactor::contingencyTableBF(sampleType = "indepMulti", fixedMargin = "cols")
    return(round(BayesFactor::extractBF(BF)$bf, 4))
  }

  top_10_oses <- names(head(sort(table(user_agents$os), decreasing = TRUE), 10))
  os_summary <- user_agents %>%
    mutate(os = if_else(os %in% top_10_oses, os, "Other OSes")) %>%
    group_by(wiki, group, os) %>%
    summarize_uas %>%
    mutate(`Bayes Factor` = purrr::map_dbl(.x = split(.[, c("wiki", "os")], seq(nrow(.))), .f = function(.x) {
      user_agents %>%
        keep_where(wiki == .x$wiki) %>%
        mutate(os = dplyr::case_when(
          os == .x$os ~ os,
          !(os %in% top_10_oses) & .x$os == "Other OSes" ~ "Other OSes",
          TRUE ~ "other")) %>%
        group_by(wiki, group, os) %>%
        get_bayes_factor
    }))
  os_summary_counts_cols_index <- which(colnames(os_summary) %in% report_params$test_group_names)

  top_10_browsers <- names(head(sort(table(user_agents$browser), decreasing = TRUE), 10))
  browser_summary <- user_agents %>%
    mutate(browser = if_else(browser %in% top_10_browsers, browser, "Other browsers")) %>%
    group_by(wiki, group, browser) %>%
    summarize_uas %>%
    mutate(`Bayes Factor` = purrr::map_dbl(.x = split(.[, c("wiki", "browser")], seq(nrow(.))), .f = function(.x) {
      user_agents %>%
        keep_where(wiki == .x$wiki) %>%
        mutate(browser = dplyr::case_when(
          browser == .x$browser ~ browser,
          !(browser %in% top_10_browsers) & .x$browser == "Other browsers" ~ "Other browsers",
          TRUE ~ "other")) %>%
        group_by(wiki, group, browser) %>%
        get_bayes_factor
    }))
  browser_summary_counts_cols_index <- which(colnames(browser_summary) %in% report_params$test_group_names)

  rm(user_agents, top_10_oses, top_10_browsers)

}
