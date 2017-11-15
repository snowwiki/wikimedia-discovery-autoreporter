set.seed(0)

suppressPackageStartupMessages({
  library(magrittr)
  library(ggplot2)
  import::from(
    # We don't import certain verbs (e.g. distinct, left_join, bind_rows)
    # to avoid potential name-conflicts and because they're one-time use.
    dplyr,
    # Subsetting Verbs
    keep_where = filter, select,
    # Grouping Verbs
    group_by, ungroup,
    # Manipulation Verbs
    mutate, arrange, summarize, tally,
    # Utility Functions
    case_when, if_else
  )
})
source("functions.R")

fig_width <- 10
fig_height <- 6
plot_resolution <- 192

# SQL setup
is_stat_machine <- grepl("^stat1", Sys.info()["nodename"])
if (!is_stat_machine & is.null(report_params$data)) {
  message("Create an auto-closing SSH tunnel in the background...")
  # See https://gist.github.com/scy/6781836 for more info.
  system("ssh -f -o ExitOnForwardFailure=yes stat1006.eqiad.wmnet -L 3307:analytics-store.eqiad.wmnet:3306 sleep 10")
  library(RMySQL)
  con <- dbConnect(MySQL(), host = "127.0.0.1", group = "client", dbname = "log", port = 3307)
}
