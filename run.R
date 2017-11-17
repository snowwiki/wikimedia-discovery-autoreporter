#!/usr/bin/env Rscript

if (Sys.info()["nodename"] == "stat1005") {
  .libPaths("/srv/home/chelsyx/R/x86_64-pc-linux-gnu-library/3.3")
}

# Check dependencies:
dependencies <- c(
  "tidyverse", "toOrdinal", "jsonlite", "yaml", "rmarkdown", "wmf", "tools",
  "knitr", "RMySQL", "data.table", "lubridate", "binom", "BCDA", "survival",
  "survminer", "polloi", "import", "BayesFactor", "formattable", "DT",
  "htmltools", "scales", "Rcpp", "urltools", "rlang", "RColorBrewer"
)

installed <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)
if (any(!dependencies %in% unname(installed$Package))) {
  stop("The following R package(s) are required but are missing: ",
       paste0(dependencies[!dependencies %in% installed$Package], collapse = ", "))
}

# Get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set defaults:
suppressPackageStartupMessages(library("optparse"))
option_list <- list(
  make_option("--yaml_file", default = "parameters.yaml", action = "store", type = "character",
              help = "A yaml file with test parameters is needed to generate the report. Default to 'parameters.yaml'."),
  make_option("--publish", default = FALSE, action = "store_true",
              help = "Whether to publish the report to analytics.wikimedia.org/datasets/discovery/reports"),
  make_option("--username", default = NULL, action = "store", type = "character",
              help = "Your LDAP username is needed to publish the report from local machine.")
)
opt <- parse_args(OptionParser(option_list = option_list))

# Set up
report_params <- yaml::yaml.load_file(opt$yaml_file)
report_params <- yaml::yaml.load_file("reports/ltr_test_18lang.yaml")
if (!dir.exists("reports")) {
  dir.create("reports")
}
fig_path <- file.path("reports", "figures")
dir.create(fig_path) # create a temp directory to save figures for the report
output_report_name <- file.path("reports", paste0(gsub("[^a-zA-Z0-9]", "_", report_params$report_title), ".html"))
source("modules/setup.R")

# Fetch data
source("modules/data/fetch_data.R")
source("modules/data/data_cleansing.R")
source("modules/data/data_aggregation.R")

# Test summary
source("modules/test_summary/events.R")
source("modules/test_summary/searches.R")
source("modules/test_summary/browser_os.R")

# Sister search
source("modules/sister_search/sidebar_results.R")
source("modules/sister_search/ssclicks.R")
source("modules/sister_search/iwclicks.R")

# Explore similar
source("modules/explore_similar/esclicks.R")
source("modules/explore_similar/hover_over.R")

# Interleaved test
source("modules/interleaved_test/data_processing.R")
source("modules/interleaved_test/interleaved_preference.R")
source("modules/interleaved_test/page_dwelltime.R")

# Statistical test (traditional)
source("modules/stat_test/remove_interleaved_data.R")
source("modules/stat_test/zrr.R")
source("modules/stat_test/engagement.R")
source("modules/stat_test/first_clicked.R")
source("modules/stat_test/max_clicked.R")
source("modules/stat_test/paulscore.R")
source("modules/stat_test/serp_offset.R")
source("modules/stat_test/visited_page.R")
source("modules/stat_test/search_abandon_rate.R")
source("modules/stat_test/serp_from_autocomplete.R")
source("modules/stat_test/return_rate.R")
source("modules/stat_test/serp_load_time.R")

# Render report
rmarkdown::render("report.Rmd", output_file = output_report_name, params = report_params)
if (!report_params$debug) {
  unlink(file.path("reports", "figures"), recursive = TRUE) # remove the temp figures directory
}

# Publish
if (opt$publish) {
  tryCatch({
    if (Sys.info()["nodename"] == "stat1005") {
      system(paste0("cp ", output_report_name, " /srv/published-datasets/discovery/reports"))
    } else {
      if (is.null(opt$username)) {
        stop("Your LDAP username is needed to publish the report from local machine!")
      }
      system(paste0("scp ", output_report_name, " ", opt$username, "@stat1005.eqiad.wmnet:/srv/published-datasets/discovery/reports"), intern = TRUE)
    }
  },
  warning = function(w) {
    stop("Publish failed! The HTML report could be found in the '/reports' directory.")
  }
  )
  message("You can view the report at https://analytics.wikimedia.org/datasets/discovery/reports/", basename(output_report_name))
}
