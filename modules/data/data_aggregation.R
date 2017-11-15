message("Check scroll on SERPs...")
events <- events %>%
  keep_where(!(event %in% c("visitPage", "checkin"))) %>%
  group_by(session_id, page_id) %>%
  summarize(n_scroll_serp = sum(event_scroll)) %>%
  ungroup %>%
  dplyr::right_join(events, by = c("session_id", "page_id"))

message("Aggregating by search...")
searches <- events %>%
  keep_where(!(is.na(serp_id))) %>% # remove visitPage and checkin events
  arrange(date, session_id, serp_id, timestamp) %>%
  group_by(group, wiki, session_id, serp_id) %>%
  summarize(
    date = date[1],
    timestamp = timestamp[1],
    # from_autocomplete = dplyr::if_else(is.null(from_autocomplete), NA, sum(from_autocomplete, na.rm = TRUE) > 0),
    has_scroll_serp = sum(n_scroll_serp, na.rm = TRUE) > 0,
    `got same-wiki results` = any(`some same-wiki results` == "TRUE", na.rm = TRUE),
    engaged = any(event != "searchResultPage") || length(unique(page_id[event == "searchResultPage"])) > 1 || any(has_scroll_serp),
    `same-wiki clickthrough` = "click" %in% event,
    `other clickthrough` = sum(grepl("click", event) & event != "click"),
    `no. same-wiki results clicked` = length(unique(event_position[event == "click"])),
    `first clicked same-wiki results position` = ifelse(`same-wiki clickthrough`, event_position[event == "click"][1], NA), # event_position is 0-based
    `max clicked position (same-wiki)` = ifelse(`same-wiki clickthrough`, max(event_position[event == "click"], na.rm = TRUE), NA),
    `Query score (F=0.1)` = query_score(event_position, 0.1),
    `Query score (F=0.5)` = query_score(event_position, 0.5),
    `Query score (F=0.9)` = query_score(event_position, 0.9)
  ) %>%
  ungroup

message("Aggregating by visited page (after clickthrough)...")
visitPage_action <- all(c("visitPage", "checkin") %in% events$event)
if (visitPage_action) {
  visitedPages <- events %>%
    arrange(date, session_id, page_id, timestamp) %>%
    group_by(group, wiki, session_id, page_id) %>%
    keep_where("visitPage" %in% event) %>% # keep only checkin and visitPage action
    summarize(
      timestamp = timestamp[1],
      position = na.omit(event_position)[1][1],
      dwell_time = ifelse("checkin" %in% event, max(event_checkin, na.rm = TRUE), 0),
      scroll = sum(event_scroll) > 0,
      status = ifelse(dwell_time == "420", 1, 2)
    ) %>%
    ungroup
  visitedPages$dwell_time[is.na(visitedPages$dwell_time)] <- 0
}

# Parse extra parameters

if ("searchResultPage" %in% events$event & !is.null(events$event_extraParams)) {

  message("Processing SERP offset data...")
  serp_offset <- events %>%
    keep_where(event == "searchResultPage", `some same-wiki results` == "TRUE") %>%
    # SERPs with 0 results will not have an offset in extraParams ^
    mutate(offset = purrr::map_int(event_extraParams, ~ parse_extraParams(.x, action = "searchResultPage")$offset)) %>%
    select(session_id, event_id, serp_id, offset)

  message("Processing SERP interwiki data...")
  extract_iw <- function(session_id, event_id, serp_id, event_extraParams) {
    return(data.frame(
      session_id, event_id, serp_id,
      parse_extraParams(event_extraParams, action = "searchResultPage")$iw,
      stringsAsFactors = FALSE
    ))
  }
  serp_iw <- events %>%
    keep_where(event == "searchResultPage") %>%
    select(session_id, event_id, serp_id, event_extraParams) %>%
    purrr::pmap_df(extract_iw) %>%
    mutate(source = case_when(
      source == "wikt" ~ "Wiktionary",
      source == "b" ~ "Wikibooks",
      source == "n" ~ "Wikinews",
      source == "q" ~ "Wikiquote",
      source == "s" ~ "Wikisource",
      source == "v" ~ "Wikiversity",
      source == "voy" ~ "Wikivoyage",
      TRUE ~ source
    ))
}

message("Processing sister-search clicks...")
if ("ssclick" %in% events$event & !is.null(events$event_extraParams)) {
  ssclick_des <- events %>%
    keep_where(event == "ssclick") %>%
    select(session_id, event_id, event_extraParams) %>%
    mutate(domain = urltools::domain(event_extraParams)) %>%
    mutate(
      language = sub("^([a-z]{2}|commons)\\.(wik[a-z]+)\\.org$", "\\1", domain),
      project = sub("^([a-z]{2}|commons)\\.(wik[a-z]+)\\.org$", "\\2", domain)
    ) %>%
    mutate(
      language = ifelse(language == "commons", NA, language),
      project = ifelse(language == "commons", "commons", project)
    ) %>%
    select(-event_extraParams, -domain)
}

message("Processing explore-similar clicks...")
if ("esclick" %in% events$event & !is.null(events$event_extraParams)) {
  esclick_result <- events %>%
    keep_where(event == "esclick") %>%
    cbind(purrr::map_df(.$event_extraParams, parse_extraParams, action = "esclick")) %>%
    select(group, wiki, session_id, serp_id, page_id, event_id, hoverId, section, result, position)

  searches <- esclick_result %>%
    group_by(group, wiki, session_id, serp_id) %>%
    summarize(with_esclick = TRUE) %>%
    dplyr::right_join(searches, by = c("group", "wiki", "session_id", "serp_id")) %>%
    dplyr::mutate(with_esclick = ifelse(is.na(with_esclick), FALSE, with_esclick))
}

message("Processing explore-similar hover events...")
if (all(c("hover-on", "hover-off") %in% events$event) & !is.null(events$event_extraParams)) {
  hover_over <- events %>%
    keep_where(event %in% c("hover-on", "hover-off")) %>%
    cbind(purrr::map_df(.$event_extraParams, parse_extraParams, action = c("hover-on", "hover-off"))) %>%
    select(timestamp, group, wiki, session_id, serp_id, page_id, event_id, event, hoverId, section, results)

  searches <- hover_over %>%
    keep_where(event == "hover-on") %>%
    group_by(group, wiki, session_id, serp_id) %>%
    summarize(n_hover = length(unique(hoverId))) %>%
    dplyr::right_join(searches, by = c("group", "wiki", "session_id", "serp_id")) %>%
    dplyr::mutate(n_hover = ifelse(is.na(n_hover), 0, n_hover))
}
