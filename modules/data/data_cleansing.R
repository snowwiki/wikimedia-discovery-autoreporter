# Event data cleansing
if (exists("events_raw")) {
  events <- events_raw
}

min_date <- format(lubridate::ymd_hms(min(events_raw$timestamp)), "%d %B %Y")
max_date <- format(lubridate::ymd_hms(max(events_raw$timestamp)), "%d %B %Y")

message("De-duplicating events...")
events <- events %>%
  mutate(
    timestamp = lubridate::ymd_hms(timestamp),
    date = as.Date(timestamp)
  ) %>%
  arrange(session_id, event_id, timestamp) %>%
  dplyr::distinct(session_id, event_id, .keep_all = TRUE)
data_cleansing_info <- paste0("Fulltext search events: Deleted ", nrow(events_raw) - nrow(events), " duplicated events.")
rm(events_raw) # to free up memory

message("Delete events with negative load time...")
data_cleansing_info <- paste0(data_cleansing_info, " Deleted ", sum(events$load_time < 0, na.rm = TRUE), " events with negative load time.")
events <- events %>%
  keep_where(is.na(load_time) | load_time >= 0)

message("De-duplicating SERPs...")
SERPs <- events %>%
  keep_where(event == "searchResultPage") %>%
  select(c(session_id, page_id, query_hash, search_token)) %>%
  group_by(session_id, query_hash) %>%
  mutate(serp_id = page_id[1], cirrus_id = search_token[1]) %>%
  ungroup %>%
  select(c(page_id, serp_id, cirrus_id))
events <- events %>%
  dplyr::left_join(SERPs, by = "page_id")
rm(SERPs) # to free up memory

message("Removing events without an associated SERP (orphan clicks and check-ins)...")
n_evnt <- nrow(events)
events <- events %>%
  keep_where(!(is.na(serp_id) & !(event %in% c("visitPage", "checkin")))) %>% # remove orphan click
  group_by(session_id) %>%
  keep_where("searchResultPage" %in% event) %>% # remove orphan "visitPage" and "checkin"
  ungroup
data_cleansing_info <- paste0(data_cleansing_info, " Removed ", n_evnt - nrow(events), " orphan (SERP-less) events.")
rm(n_evnt)

message("Removing sessions falling into multiple test groups")
temp <- events %>%
  group_by(session_id) %>%
  summarize(unique_group = length(unique(group)) == 1)
data_cleansing_info <- paste0(data_cleansing_info, " Removed ", sum(!temp$unique_group), " sessions falling into multiple test groups.")
events <- events %>%
  keep_where(session_id %in% temp$session_id[temp$unique_group])
rm(temp)

message("Remove sessions with more than 100 searches...")
spider_session <- events %>%
  group_by(date, group, session_id) %>%
  summarize(n_search = length(unique(serp_id))) %>%
  keep_where(n_search > 100) %>%
  {.$session_id}
events <- events %>%
  keep_where(!(session_id %in% spider_session))
data_cleansing_info <- paste0(data_cleansing_info, " Removed ", length(spider_session), " sessions with more than 100 searches.")
rm(spider_session)

# Number of wikis in the test
n_wiki <- length(unique(events$wiki))
message("Number of wikis in the test: ", n_wiki)

# Number of wikis with iwclick
iwclick_wiki <- length(unique(events$wiki[events$event == "iwclick"]))
iwclick_wiki_names <- unique(events$wiki[events$event == "iwclick"])


# Cleansing fulltext search events from autocomplete search
if (exists("fulltext_from_auto")) {
  message("Cleansing fulltext SERP data that come from autocomplete...")

  message("De-duplicating events...")
  n_fulltext_from_auto <- nrow(fulltext_from_auto)
  fulltext_from_auto <- fulltext_from_auto %>%
    mutate(
      timestamp = lubridate::ymd_hms(timestamp),
      date = as.Date(timestamp)
    ) %>%
    arrange(session_id, event_id, timestamp) %>%
    dplyr::distinct(session_id, event_id, .keep_all = TRUE)
  fulltext_from_auto_cleansing_info <- paste0("SERP check-in events: Deleted ", n_fulltext_from_auto - nrow(fulltext_from_auto), " duplicated events.")
  rm(n_fulltext_from_auto) # to free up memory

  message("Group by SERPs...")
  serp_from_auto <- fulltext_from_auto %>%
    arrange(date, session_id, page_id, timestamp) %>%
    group_by(wiki, group, session_id, page_id) %>%
    summarize(from_autocomplete = TRUE,
              max_checkin_serp = ifelse("checkin" %in% event, max(event_checkin, na.rm = TRUE), 0),
              status = ifelse(max_checkin_serp == "420", 1, 2)
    )

  message("Join with events...")
  events <- events %>%
    dplyr::left_join(serp_from_auto, by = c("group", "wiki", "session_id", "page_id")) %>%
    mutate(from_autocomplete = ifelse(is.na(from_autocomplete), FALSE, from_autocomplete))
  # some serp from auto couldn't find a match because their sessions are identified as spider sessions
  fulltext_from_auto_cleansing_info <- paste0(fulltext_from_auto_cleansing_info, " There are ", length(unique(events$page_id[events$from_autocomplete])), " SERPs from autocomplete.")
}
