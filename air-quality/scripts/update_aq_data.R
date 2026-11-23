library(arcgislayers)
library(dplyr)
library(lubridate)
library(tibble)
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#
schema_helper <- if (file.exists("../shared/R/schema_utils.R")) {
  "../shared/R/schema_utils.R"
} else {
  "shared/R/schema_utils.R"
}


source(schema_helper)
source("R/get_deq_pm25.R")
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#


dir.create("data", showWarnings = FALSE, recursive = TRUE)
hourly_path <- "data/aq_hourly.rds"
daily_path  <- "data/aq_daily.rds"

now_mt   <- with_tz(Sys.time(), "America/Denver")
today_mt <- as_date(now_mt)
active_fields <- fields_pull

has_existing <- file.exists(hourly_path) && file.exists(daily_path)

if (!has_existing) {
  
#----------------------------------------------------------------------------#
# First-run bootstrap: pull rolling 12-month window from DEQ
#----------------------------------------------------------------------------#
  message("No existing archive found. Running initial 12-month backfill...")
  
  fresh <- get_deq_pm25(
    start_date  = today_mt - months(12),
    end_date    = today_mt,
    fields      = active_fields,
    where_base  = where_sql,
    output      = "both",
    publish_lag = minutes(20),
    verbose     = TRUE
  )
  
  if (is.null(fresh)) {
    stop("Initial DEQ backfill returned no records; aborting publish.")
  }
  
  aq_hourly_final <- fresh$hourly %>% arrange(date_mt)
  aq_daily_final  <- fresh$daily  %>% arrange(desc(date_clean))
  
} else {

#----------------------------------------------------------------------------#
# Routine hourly run: recompute & overwrite the last 7 calendar days
# (fetches 8 days so lag() has a valid neighbor on day 1 of the 7-day slice)
#----------------------------------------------------------------------------#
  overwrite_from <- today_mt - days(6)
  fetch_start    <- today_mt - days(7)
  
  message("Refreshing rolling window: ", fetch_start, " to ", today_mt)
  
  fresh <- get_deq_pm25(
    start_date  = fetch_start,
    end_date    = today_mt,
    fields      = active_fields,
    where_base  = where_sql,
    output      = "both",
    publish_lag = minutes(20),
    verbose     = TRUE
  )
  
#----------------------------------------------------------------------------#  
# Fail workflow without touching the existing release if DEQ is unreachable
  if (is.null(fresh)) {
    stop("DEQ pull returned NULL; preserving existing release assets.")
  }
  
  old_hourly <- readRDS(hourly_path)
  old_daily  <- readRDS(daily_path)
  
  new_hourly <- fresh$hourly %>% filter(day_mt >= overwrite_from)
  new_daily  <- fresh$daily  %>% filter(date_clean >= overwrite_from)

#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#
# function in /shared/schema-utils.R to ensure matching schemas 
  assert_same_schema(old_hourly, new_hourly, "aq_hourly")
  assert_same_schema(old_daily,  new_daily,  "aq_daily")
#----------------------------------------------------------------------------#  
#----------------------------------------------------------------------------#
  aq_hourly_final <- old_hourly %>%
    filter(day_mt < overwrite_from) %>%
    bind_rows(new_hourly) %>%
    arrange(date_mt)
  
  aq_daily_final <- old_daily %>%
    filter(date_clean < overwrite_from) %>%
    bind_rows(new_daily) %>%
    arrange(desc(date_clean))
}

#----------------------------------------------------------------------------#
# Sanity check before saving
stopifnot(nrow(aq_hourly_final) > 0L, nrow(aq_daily_final) > 0L)

saveRDS(aq_hourly_final, hourly_path)
saveRDS(aq_daily_final,  daily_path)

message(sprintf(
  "Saved %d hourly rows (%s to %s) and %d daily rows.",
  nrow(aq_hourly_final),
  min(aq_hourly_final$day_mt, na.rm = TRUE),
  max(aq_hourly_final$day_mt, na.rm = TRUE),
  nrow(aq_daily_final)
))