query <- paste0("SELECT
    timestamp,
    event_uniqueId AS event_id,
    event_mwSessionId,
    event_pageViewId AS page_id,
    event_articleId AS article_id,
    event_searchSessionId AS session_id,
    event_subTest AS `group`,
    wiki,
    MD5(LOWER(TRIM(event_query))) AS query_hash,
    event_action AS event,
    CASE
    WHEN event_position < 0 THEN NULL
    ELSE event_position
    END AS event_position,
    CASE
    WHEN event_action = 'searchResultPage' AND event_hitsReturned > 0 THEN 'TRUE'
    WHEN event_action = 'searchResultPage' AND event_hitsReturned IS NULL THEN 'FALSE'
    ELSE NULL
    END AS `some same-wiki results`,
    CASE
    WHEN event_action = 'searchResultPage' AND event_hitsReturned > -1 THEN event_hitsReturned
    WHEN event_action = 'searchResultPage' AND event_hitsReturned IS NULL THEN 0
    ELSE NULL
    END AS n_results,
    event_scroll,
    event_checkin,
    event_extraParams,
    event_msToDisplayResults AS load_time,
    userAgent AS user_agent
  FROM TestSearchSatisfaction2_", report_params$tss2_revision, "\n",
  "WHERE LEFT(timestamp, 8) >= '", report_params$start_date, "' AND LEFT(timestamp, 8) < '", report_params$end_date, "' \n",
  ifelse(is.null(report_params$wiki), "", paste0("  AND wiki IN('", paste(report_params$wiki, collapse = "', '"), "') \n")),
  ifelse(is.null(report_params$test_group_names), "", paste0("  AND event_subTest IN('", paste(report_params$test_group_names, collapse = "', '"), "') \n")),
  ifelse(is.null(report_params$event_action), "", paste0("  AND event_action IN('", paste(report_params$event_action, collapse = "', '"), "') \n")),
  ifelse(is.null(report_params$event_source), "", paste0("  AND event_source IN('", paste(report_params$event_source, collapse = "', '"), "') \n")),
  ifelse(is.null(report_params$other_filter), "", paste0("  AND ", report_params$other_filter, " \n")),
  " AND INSTR(userAgent, '\"is_bot\": false') > 0
    AND CASE WHEN event_action = 'searchResultPage' THEN event_msToDisplayResults IS NOT NULL
      WHEN event_action IN ('click', 'iwclick', 'ssclick') THEN event_position IS NOT NULL AND event_position > -1
      WHEN event_action = 'visitPage' THEN event_pageViewId IS NOT NULL
      WHEN event_action = 'checkin' THEN event_checkin IS NOT NULL AND event_pageViewId IS NOT NULL
      ELSE TRUE
      END;"
)

# Fetch full-text search from autocomplete search to compute dwell time on SERP
query_autocomplete <- paste0("SELECT
    timestamp,
    event_uniqueId AS event_id,
    event_mwSessionId,
    event_pageViewId AS page_id,
    event_searchSessionId AS session_id,
    event_subTest AS `group`,
    wiki,
    event_action AS event,
    event_scroll,
    event_checkin,
    event_searchToken AS search_token,
    userAgent AS user_agent
  FROM TestSearchSatisfaction2_", report_params$tss2_revision, "\n",
  "WHERE LEFT(timestamp, 8) >= '", report_params$start_date, "' AND LEFT(timestamp, 8) < '", report_params$end_date, "' \n",
  ifelse(is.null(report_params$wiki), "", paste0("  AND wiki IN('", paste(report_params$wiki, collapse = "', '"), "') \n")),
  ifelse(is.null(report_params$test_group_names), "", paste0("  AND event_subTest IN('", paste(report_params$test_group_names, collapse = "', '"), "') \n")),
  ifelse(is.null(report_params$other_filter), "", paste0("  AND ", report_params$other_filter, " \n")),
  " AND event_source = 'autocomplete'
    AND event_articleId IS NULL
    AND event_action IN('visitPage', 'checkin')
    AND CASE WHEN event_action = 'searchResultPage' THEN event_msToDisplayResults IS NOT NULL
      WHEN event_action IN ('click', 'iwclick', 'ssclick') THEN event_position IS NOT NULL AND event_position > -1
      WHEN event_action = 'visitPage' THEN event_pageViewId IS NOT NULL
      WHEN event_action = 'checkin' THEN event_checkin IS NOT NULL AND event_pageViewId IS NOT NULL
      ELSE TRUE
      END;"
)

if (!is.null(report_params$query)) {
  message("Using user-supplied query instead of built-in query...")
  query <- report_params$query
}

if (is.null(report_params$data)) {

  message("User did not provide data. Fetching data using query...")
  if (is_stat_machine) {
    message("(Running on a stat machine.)")
    events_raw <- wmf::mysql_read(query, "log")
    if (report_params$serp_dwell_time) {
      fulltext_from_auto <- wmf::mysql_read(query_autocomplete, "log")
    }
  } else {
    message("Using SSH tunnel & connection to Analytics-Store...")
    events_raw <- wmf::mysql_read(query, "log", con = con)
    if (report_params$serp_dwell_time) {
      fulltext_from_auto <- wmf::mysql_read(query_autocomplete, "log", con = con)
    }
    message("Closing connection...")
    wmf::mysql_close(con)
  }

  if (!dir.exists(file.path("data", gsub(.Platform$file.sep, "", report_params$report_title)))) {
    message("Data directory does not exist; creating one...")
    dir.create(file.path("data", gsub(.Platform$file.sep, "", report_params$report_title)), recursive = TRUE)
  }

  message("Saving raw events data...")
  save(events_raw, file = file.path("data", gsub(.Platform$file.sep, "", report_params$report_title), paste0("events_raw_", gsub("[^0-9]", "", Sys.time()), ".RData")))

  if (report_params$serp_dwell_time) {
    message("Saving SERP data that are from autocomplete...")
    save(fulltext_from_auto, file = file.path("data", gsub(.Platform$file.sep, "", report_params$report_title), paste0("fulltext_from_auto_", gsub("[^0-9]", "", Sys.time()), ".RData")))
  }

} else {

  if (tools::file_ext(report_params$data) == "RData") {
    load(report_params$data)
  } else{
    events_raw <- data.table::fread(input = report_params$data, data.table = FALSE)
  }

}
