if (!is.null(report_params$interleaved_group_names)) {
  message("Remove interleaved data...")

  events <- events[!(events$group %in% report_params$interleaved_group_names), ]
  searches <- searches[!(searches$group %in% report_params$interleaved_group_names), ]

   if (all(c("visitPage", "checkin") %in% events$event)) {
    visitedPages <- visitedPages[!(visitedPages$group %in% report_params$interleaved_group_names), ]
   }

  if (exists("fulltext_from_auto")) {
    fulltext_from_auto <- fulltext_from_auto[!(fulltext_from_auto$group %in% report_params$interleaved_group_names), ]
    serp_from_auto <- serp_from_auto[!(serp_from_auto$group %in% report_params$interleaved_group_names), ]
  }

}

traditional_test_groups <- setdiff(report_params$test_group_names, report_params$interleaved_group_names)
