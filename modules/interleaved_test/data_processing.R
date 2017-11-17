if (!is.null(report_params$interleaved_group_names)) {

  message("Separate interleaved dataset")
  events_interleaved <- events[events$group %in% report_params$interleaved_group_names, ]
  n_interleaved_groups <- length(report_params$interleaved_group_names)

  message("Extracting team draft data so we know which visited result is which")
  events_interleaved %<>%
    keep_where(events_interleaved$event %in% c("searchResultPage", "click", "visitPage", "checkin")) %>%
    dplyr::arrange(wiki, group, session_id, timestamp, event) %>%
    mutate(search_id = fill_in(search_id))
  events_interleaved <- data.table::data.table(events_interleaved)
  events_interleaved[, team := process_session(.SD), by = c("wiki", "group", "session_id"), .SDcols = c("serp_id", "event_extraParams", "article_id")]
  events_interleaved <- events_interleaved[order(events_interleaved$wiki, events_interleaved$group, events_interleaved$session_id, events_interleaved$serp_id, events_interleaved$timestamp, events_interleaved$event), ]
  events_interleaved[, event_extraParams := NULL, ]

  message("Remove sessions with large numbers of searches")
  events_interleaved[, valid := length(unique(search_id)) <= 20, by = c("date", "wiki", "group", "session_id")]
  interleaved_visitpage <- events_interleaved[
    !is.na(team) & team != "" & event == "visitPage" & valid == TRUE,
    c("date", "wiki", "group", "session_id", "search_id", "team"),
    with = TRUE
    ]
  interleaved_visitpage <- interleaved_visitpage[order(interleaved_visitpage$date, interleaved_visitpage$wiki, interleaved_visitpage$group, interleaved_visitpage$session_id, interleaved_visitpage$search_id, interleaved_visitpage$team), ]

}
