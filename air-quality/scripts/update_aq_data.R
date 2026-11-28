library(EpivalE)
library(dplyr)
library(lubridate)

#------------------------------------------------------------------------------#
dir.create("data", showWarnings = FALSE, recursive = TRUE)
hourly_path <- "data/aq_hourly.rds"
daily_path  <- "data/aq_daily.rds"

now_mt   <- with_tz(Sys.time(), "America/Denver")
today_mt <- as_date(now_mt)

#------------------------------------------------------------------------------#
# Fetch weather data using httr2 -> openmeteo
#------------------------------------------------------------------------------#
wx_cols <- setdiff(names(wx_empty_daily()), "date_clean")

attach_weather <- function(daily_df, start_d, end_d, old_daily = NULL) {
  wx <- wx_get_daily(start_date = start_d, end_date = end_d)
  
  if (!is.null(wx) && nrow(wx) > 0L) {
    return(daily_df %>% left_join(wx, by = "date_clean"))
  }
  
  warning("Open-Meteo unavailable; preserving prior weather values where available.")
  
  if (!is.null(old_daily) && all(wx_cols %in% names(old_daily))) {
    prior_wx <- old_daily %>% select(date_clean, all_of(wx_cols))
    return(daily_df %>% left_join(prior_wx, by = "date_clean"))
  }
  
  daily_df %>% left_join(wx_empty_daily(daily_df$date_clean), by = "date_clean")
}

has_existing <- file.exists(hourly_path) && file.exists(daily_path)

if (!has_existing) {
  
  #----------------------------------------------------------------------------#
  # First-run bootstrap: pull rolling 12-month window
  #----------------------------------------------------------------------------#
  message("No existing archive found. Running initial 12-month backfill...")
  
  backfill_start <- today_mt - months(12)
  
  fresh <- aq_get_deq_pm25(
    start_date  = backfill_start,
    end_date    = today_mt,
    output      = "both",
    publish_lag = minutes(20),
    verbose     = TRUE
  )
  
  if (is.null(fresh)) {
    stop("Initial DEQ backfill returned no records; aborting publish.")
  }
  
  aq_hourly_final <- fresh$hourly %>% arrange(date_mt)
  aq_daily_final  <- fresh$daily %>%
    attach_weather(backfill_start, today_mt) %>%
    arrange(desc(date_clean))
  
} else {
  
  #----------------------------------------------------------------------------#
  # Routine run: recompute & overwrite the last 7 calendar days
  # (fetches 8 days so lag() has a valid neighbor on day 1 of the 7-day slice)
  #----------------------------------------------------------------------------#
  overwrite_from <- today_mt - days(6)
  fetch_start    <- today_mt - days(7)
  
  message("Refreshing rolling window: ", fetch_start, " to ", today_mt)
  
  fresh <- aq_get_deq_pm25(
    start_date  = fetch_start,
    end_date    = today_mt,
    output      = "both",
    publish_lag = minutes(20),
    verbose     = TRUE
  )
  
  # Fail workflow without touching the existing release if DEQ is unreachable
  if (is.null(fresh)) {
    stop("DEQ pull returned NULL; preserving existing release assets.")
  }
  
  old_hourly <- readRDS(hourly_path)
  old_daily  <- readRDS(daily_path)
  
  new_hourly <- fresh$hourly %>% filter(date_clean >= overwrite_from)
  new_daily  <- fresh$daily %>%
    filter(date_clean >= overwrite_from) %>%
    attach_weather(overwrite_from, today_mt, old_daily = old_daily)
  
  # Schema guard from EpivalE
  assert_same_schema(old_hourly, new_hourly, "aq_hourly")
  assert_same_schema(old_daily,  new_daily,  "aq_daily")
  
  aq_hourly_final <- old_hourly %>%
    filter(date_clean < overwrite_from) %>%
    bind_rows(new_hourly) %>%
    arrange(date_mt)
  
  aq_daily_final <- old_daily %>%
    filter(date_clean < overwrite_from) %>%
    bind_rows(new_daily) %>%
    arrange(desc(date_clean))
}

#------------------------------------------------------------------------------#
# Sanity check before saving
#------------------------------------------------------------------------------#
stopifnot(nrow(aq_hourly_final) > 0L, nrow(aq_daily_final) > 0L)

saveRDS(aq_hourly_final, hourly_path)
saveRDS(aq_daily_final,  daily_path)

message(sprintf(
  "Saved %d hourly rows (%s to %s) and %d daily rows.",
  nrow(aq_hourly_final),
  min(aq_hourly_final$date_clean, na.rm = TRUE),
  max(aq_hourly_final$date_clean, na.rm = TRUE),
  nrow(aq_daily_final)
))