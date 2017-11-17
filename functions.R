# PaulScore Calculation
# 0-based positions
query_score <- function(positions, F) {
  if (length(positions) == 1 || all(is.na(positions))) {
    # no clicks were made
    return(0)
  } else {
    positions <- unique(positions[!is.na(positions)])
    # when operating on 'events' dataset, searchResultPage events won't have positions
    # sometimes users may click on the same position multiple times, see https://phabricator.wikimedia.org/T172960
    return(sum(F ^ positions))
  }
}

# Bootstrapping
bootstrap_mean <- function(x, m, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  n <- length(x)
  return(replicate(m, mean(x[sample.int(n, n, replace = TRUE)])))
}

# String utils
safe_ordinals <- function(x) {
  na_mask <- is.na(x)
  output <- rep(NA, length(x))
  output[!na_mask] <- vapply(x[!na_mask], toOrdinal::toOrdinal, "")
  return(output)
}
pluralize <- function(singular, n) {
  plural <- paste0(singular, "s") # TODO: logic for ending
  return(c(singular, plural)[(n != 1) + 1])
}
Pluralize <- function(n, singular) {
  return(paste(n, pluralize(singular, n)))
}

# Parse extraParams
parse_extraParams <- function(extraParams, action){
  if (extraParams == "{}") {
    if (all(action %in% c("hover-on", "hover-off"))) {
      return(list(hoverId = NA, section = NA, results = NA))
    } else if (all(action %in% c("esclick"))) {
      return(list(hoverId = NA, section = NA, result = NA))
    } else if (all(action %in% c("searchResultPage"))) {
      return(list(offset = NA, iw = list(source = NA, position = NA)))
    } else {
      return(NA)
    }
  } else {
    if (all(action %in% c("searchResultPage"))) {
      output <- jsonlite::fromJSON(txt = as.character(extraParams), simplifyVector = TRUE)
      offset <- polloi::data_select(is.null(output$offset), NA, output$offset)
      iw <- polloi::data_select(is.null(output$iw), list(source = NA, position = NA), output$iw)
      return(list(offset = offset, iw = iw))
    } else {
      # "hover-on", "hover-off", "esclick"
      return(jsonlite::fromJSON(txt = as.character(extraParams), simplifyVector = TRUE))
    }
  }
}

# Bar chart
bar_chart <- function(data = NULL, x, y, geom_text_size = 3, x_lab = NULL, y_lab = NULL, title = NULL, caption = NULL, subtitle = NULL, ...) {
  ggplot2::ggplot(data = data, aes_string(x = x, y = y, fill = "group")) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_brewer("Group", palette = "Set1") +
    ggplot2::scale_y_continuous(labels = polloi::compress) +
    ggplot2::geom_text(aes_string(label = y, vjust = -0.05), position = position_dodge(width = 1), size = geom_text_size) +
    ggplot2::labs(y = y_lab, x = x_lab, title = title, subtitle = subtitle, caption = caption)
}

# Point range chart
pointrange_chart <- function(data = NULL, y_lab = NULL, title = NULL, caption = NULL, subtitle = NULL, ...) {
  ggplot2::ggplot(data = data, aes(x = group, color = group, y = mean, ymin = lower, ymax = upper)) +
    geom_linerange() +
    geom_label(aes(label = sprintf("%.2f%%", 100 * mean)), show.legend = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::labs(x = NULL, color = "Group", y = y_lab, title = title, subtitle = subtitle)
}

cppFunction('CharacterVector fill_in(CharacterVector ids) {
  CharacterVector new_ids(ids.size());
  String current_id = ids[0];
  new_ids[0] = current_id;
  for (int i = 1; i < ids.size(); i++) {
    if (ids[i] != NA_STRING) {
      current_id = ids[i];
    }
    new_ids[i] = current_id;
  }
  return new_ids;
}')

cppFunction('NumericVector cumunique(CharacterVector ids) {
  NumericVector count(ids.size());
  String current_id = ids[0];
  count[0] = 1;
  for (int i = 1; i < ids.size(); i++) {
    if (ids[i] == current_id) {
      count[i] = count[i-1];
    } else {
      count[i] = count[i-1] + 1;
      current_id = ids[i];
    }
  }
  return count;
}')

# Process interleaved team draft
process_session <- function(df) {
  processed_session <- unsplit(lapply(split(df, df$serp_id), function(df) {
    if (is.na(df$event_extraParams[1]) || df$event_extraParams[1] == "") {
      visited_pages <- rep(as.character(NA), times = nrow(df))
    } else {
      from_json <- jsonlite::fromJSON(df$event_extraParams[1], simplifyVector = FALSE)
      if (!("teamDraft" %in% names(from_json)) || all(is.na(df$article_id))) {
        visited_pages <- rep(as.character(NA), times = nrow(df))
      } else {
        team_a <- unlist(from_json$teamDraft$a)
        team_b <- unlist(from_json$teamDraft$b)
        visited_pages <- vapply(df$article_id, function(article_id) {
          if (article_id %in% team_a) {
            return("A")
          } else if (article_id %in% team_b) {
            return("B")
          } else {
            return(as.character(NA))
          }
        }, "")
      }
    }
    return(visited_pages)
  }), df$serp_id)
  return(processed_session)
}
