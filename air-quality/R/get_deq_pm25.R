#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
#- FETCH GLOBAL VARIABLES + OBJECTS
#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#
# SQL clause and fields to query DEQ API
#------------------------------------------------------------------------------#
# smaller field set (fastest for multi-month daily summaries)
fields_pull <- c("parameter", "instrumenttype", "date", "rawvalue")

# full field set (includes DEQ metadata, stan, nowcast, AQI, and health category)
daily_fields_pull <- c(
  "logger_id", "sitename", "aqs_code", "parameter", "instrumenttype",
  "date", "hour", "rawvalue", "stan", "nowcast", "aqi_value", "healthcategory"
)

# Base SQL WHERE clause (station + monitor channels only; dates added per chunk)
where_sql <- paste(
  "logger_id = 'RP'",
  "AND ((parameter = 'PM25' AND instrumenttype = 'Regulatory')",
  "OR (parameter = 'PM25_COL' AND instrumenttype = 'Regulatory_2'))"
)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#

get_deq_pm25 <- function(
    start_date,
    end_date    = today(tzone = "America/Denver"),
    fields      = fields_pull,
    where_base  = where_sql,
    deq_lyr     = NULL,
    chunk_days  = 14L,
    output      = c("daily", "hourly", "both"),
    publish_lag = minutes(0),
    pause_sec   = 0.25,
    verbose     = TRUE
) {
  output  <- match.arg(output)
  now_mt  <- with_tz(Sys.time(), "America/Denver")
  start_d <- as_date(start_date)
  end_d   <- as_date(end_date)
  
  # Fail-fast input checks
  if (is.na(start_d) || is.na(end_d) || start_d > end_d) {
    stop("`start_date` and `end_date` must be valid dates with start_date <= end_date.")
  }
  
  required_fields <- c("parameter", "instrumenttype", "date", "rawvalue")
  fields          <- unique(as.character(fields))
  missing_fields  <- setdiff(required_fields, fields)
  if (length(missing_fields) > 0L) {
    stop(
      "`fields` is missing required columns: ",
      paste(missing_fields, collapse = ", ")
    )
  }
  
  if (!is.character(where_base) || length(where_base) != 1L || !nzchar(trimws(where_base))) {
    stop("`where_base` must be a single, non-empty SQL WHERE clause string.")
  }
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#  
  # Internal helpers
  # 
  
  first_valid <- function(x) {
    v <- x[!is.na(x)]
    if (length(v) == 0L) NA_real_ else v[1]
  }
  
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#  
  #
  
  calc_reg_avg <- function(ts, vals, day_date, cutoff_hour, window_hrs) {
    cutoff_ts <- as.POSIXct(
      sprintf("%s %02d:00:00", day_date, cutoff_hour),
      tz = "America/Denver"
    )
    if (now_mt < cutoff_ts + publish_lag) return(NA_real_)
    
    window_vals <- vals[ts >= (cutoff_ts - dhours(window_hrs)) & ts < cutoff_ts]
    if (all(is.na(window_vals))) return(NA_real_)
    
    round(mean(window_vals, na.rm = TRUE), 2)
  }
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#  
  #
  
  count_reg_hrs <- function(ts, vals, day_date, cutoff_hour, window_hrs) {
    cutoff_ts <- as.POSIXct(
      sprintf("%s %02d:00:00", day_date, cutoff_hour),
      tz = "America/Denver"
    )
    if (now_mt < cutoff_ts + publish_lag) return(NA_integer_)
    
    sum(!is.na(vals[ts >= (cutoff_ts - dhours(window_hrs)) & ts < cutoff_ts]))
  }
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  #
  
  build_completeness_note <- function(day_complete, n_valid, n_expected,
                                      n_9am_4, n_9am_8, n_4pm_4, n_4pm_8) {
    if (day_complete && n_valid == 0L) {
      return(sprintf("No valid hourly readings recorded (0/%d hrs)", n_expected))
    }
    
    notes <- character()
    
    if (day_complete && n_valid < 18L) {
      notes <- c(
        notes,
        sprintf("24-hr stats suppressed (%d/%d valid hrs; below 18-hr minimum)", n_valid, n_expected)
      )
    }
    
    win_names   <- c("9AM 4hr", "9AM 8hr", "4PM 4hr", "4PM 8hr")
    win_counts  <- c(n_9am_4, n_9am_8, n_4pm_4, n_4pm_8)
    win_targets <- c(4L, 8L, 4L, 8L)
    
    missing_wins <- win_names[!is.na(win_counts) & win_counts == 0L]
    if (length(missing_wins) > 0L) {
      notes <- c(notes, paste0("No data for ", paste(missing_wins, collapse = ", ")))
    }
    
    partial_idx <- which(!is.na(win_counts) & win_counts > 0L & win_counts < win_targets)
    if (length(partial_idx) > 0L) {
      partial_txt <- sprintf(
        "%s (%d/%d hrs)",
        win_names[partial_idx], win_counts[partial_idx], win_targets[partial_idx]
      )
      notes <- c(notes, paste0("Partial coverage: ", paste(partial_txt, collapse = ", ")))
    }
    
    if (length(notes) == 0L) NA_character_ else paste(notes, collapse = "; ")
  }
  
  
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  # open DEQ API if not already passed in (no token!)
  if (is.null(deq_lyr)) {
    deq_lyr <- arc_open(
      "https://gis.mtdeq.us/mapping/rest/services/Montana_Air_Quality_Monitoring_Stations/FeatureServer/0",
      token = NULL
    )
  }
  
  
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  #
  # Fetch + Build the chunks!
  # 
  #
  chunk_starts <- seq(start_d, end_d, by = paste(chunk_days, "days"))
  chunk_ends   <- pmin(chunk_starts + days(chunk_days), end_d + days(1))
  n_chunks     <- length(chunk_starts)
  
  fetch_chunk <- function(c_start, c_end, max_tries = 3L) {
    chunk_where <- sprintf(
      "(%s) AND date >= TIMESTAMP '%s 00:00:00' AND date < TIMESTAMP '%s 00:00:00'",
      trimws(where_base),
      format(c_start, "%Y-%m-%d"),
      format(c_end,   "%Y-%m-%d")
    )
    
    for (attempt in seq_len(max_tries)) {
      res <- tryCatch(
        arc_select(
          deq_lyr,
          where    = chunk_where,
          fields   = fields,
          geometry = FALSE,
          token    = NULL
        ),
        error = function(e) e
      )
      
      if (!inherits(res, "error")) return(res)
      
      if (attempt < max_tries) {
        if (verbose) message("  Retry ", attempt, " for ", c_start, ": ", conditionMessage(res))
        Sys.sleep(2^attempt)
      } else {
        warning("Chunk ", c_start, " to ", c_end - days(1), " failed: ", conditionMessage(res))
        return(NULL)
      }
    }
  }
  
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#  
  # Pull all chunks and bind into one raw hourly dataframe
  batches <- vector("list", n_chunks)
  
  for (i in seq_len(n_chunks)) {
    if (verbose) {
      message(sprintf(
        "[%d/%d] Fetching %s to %s...",
        i, n_chunks, chunk_starts[i], chunk_ends[i] - days(1)
      ))
    }
    
    res <- fetch_chunk(chunk_starts[i], chunk_ends[i])
    if (!is.null(res) && nrow(res) > 0) {
      batches[[i]] <- res
    }
    
    if (i < n_chunks && pause_sec > 0) Sys.sleep(pause_sec)
  }
  
  aq_raw_hourly <- bind_rows(batches)
  
  if (nrow(aq_raw_hourly) == 0) {
    warning("No records returned for ", start_d, " to ", end_d, ".")
    return(NULL)
  }
  
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#  
  #------------------------------------------------------------------------------#
  # Now calculate stats and QA/QC checks 
  #----------------------------------------------------------------------------#
  #
  
  aq_dedup <- aq_raw_hourly %>%
    mutate(
      date_mt   = with_tz(date, "America/Denver"),
      raw_clean = pmax(rawvalue, 0)
    ) %>%
    summarise(
      reg2 = first_valid(raw_clean[parameter == "PM25_COL" & instrumenttype == "Regulatory_2"]),
      reg1 = first_valid(raw_clean[parameter == "PM25"     & instrumenttype == "Regulatory"]),
      .by = date_mt
    ) %>%
    mutate(
      raw_clean = coalesce(reg2, reg1),
      other_val = if_else(!is.na(reg2), reg1, reg2),
      parameter = case_when(
        !is.na(reg2) ~ "PM25_COL",
        !is.na(reg1) ~ "PM25",
        TRUE             ~ NA_character_
      ),
      pm25 = round(raw_clean)
    )
  
  #----------------------------------------------------------------------------#
  #----------------------------------------------------------------------------#
  #
  hour_spine <- tibble::tibble(
    date_mt = seq(
      from = as.POSIXct(sprintf("%s 00:00:00", start_d), tz = "America/Denver"),
      to   = as.POSIXct(sprintf("%s 23:00:00", end_d),   tz = "America/Denver"),
      by   = "1 hour"
    )
  )
  
  aq_hourly <- hour_spine %>%
    left_join(aq_dedup, by = "date_mt") %>%
    arrange(date_mt) %>%
    mutate(
      day_mt      = as_date(date_mt),
      hour_mt     = lubridate::hour(date_mt),
      date_end_mt = date_mt + dhours(1),
      date_clean  = as_date(date_end_mt),
      date_txt   = sub(" 0", " ", format(date_clean, "%b %d")),
      year_txt   = format(date_clean, "%Y"),
      hr_txt      = sub("^0", "", format(date_end_mt, "%I %p")),
      
      prev_val     = lag(raw_clean, 1),
      next_val     = lead(raw_clean, 1),
      neighbor_ref = case_when(
        !is.na(prev_val) & !is.na(next_val) ~ pmax(prev_val, next_val),
        !is.na(prev_val)                    ~ prev_val,
        !is.na(next_val)                    ~ next_val,
        TRUE                                ~ NA_real_
      ),
      
      flag_collocated = coalesce(
        !is.na(raw_clean) & !is.na(other_val) &
          (raw_clean - other_val) >= 35 &
          raw_clean >= 3 * pmax(other_val, 5),
        FALSE
      ),
      flag_spike = coalesce(
        !is.na(raw_clean) & !is.na(neighbor_ref) &
          raw_clean >= 50 &
          (raw_clean - neighbor_ref) >= 50 &
          raw_clean >= 3 * pmax(neighbor_ref, 5),
        FALSE
      ),
      flag_extreme = coalesce(raw_clean >= 500, FALSE),
      
      qa_flag   = flag_collocated | flag_spike | flag_extreme,
      qa_reason = case_when(
        flag_collocated & flag_spike ~ "Collocated mismatch & isolated spike",
        flag_collocated              ~ "Collocated mismatch",
        flag_spike                   ~ "Isolated 1-hr spike",
        flag_extreme                 ~ "Extreme value (>= 500)",
        TRUE                         ~ NA_character_
      ),
      
      qa_subbed = coalesce(
        parameter == "PM25_COL" & flag_collocated & flag_spike & !is.na(reg1),
        FALSE
      )
    ) %>%
    mutate(
      raw_clean = if_else(qa_subbed, reg1, raw_clean),
      pm25      = round(raw_clean),
      parameter = if_else(qa_subbed, "PM25", parameter)
    ) %>%
    select(-prev_val, -next_val, -neighbor_ref)
  
  #------------------------------------------------------------------------------# 
  #------------------------------------------------------------------------------# 
  # Re-attach any extra requested fields (e.g., daily_fields_pull) for the final
  # winning channel, skipping this join entirely when using slim `fields_pull`
  # 
  
  extra_fields <- setdiff(fields, required_fields)
  if (length(extra_fields) > 0L) {
    aq_meta <- aq_raw_hourly %>%
      mutate(date_mt = with_tz(date, "America/Denver")) %>%
      arrange(is.na(rawvalue)) %>%
      select(date_mt, parameter, any_of(extra_fields)) %>%
      distinct(date_mt, parameter, .keep_all = TRUE)
    
    aq_hourly <- aq_hourly %>%
      left_join(aq_meta, by = c("date_mt", "parameter"), na_matches = "never")
  }
  
  if (output == "hourly") return(aq_hourly)
  
  #----------------------------------------------------------------------------#  
  #----------------------------------------------------------------------------#
  # 4-hr, 8-hr, and 24-hr daily summaries + QA audit trail
  #----------------------------------------------------------------------------#
  aq_daily_avg <- aq_hourly %>%
    group_by(day_mt) %>%
    summarise(
      day_end_ts   = as.POSIXct(
        sprintf("%s 00:00:00", first(day_mt) + days(1)),
        tz = "America/Denver"
      ),
      day_complete = now_mt >= (day_end_ts + publish_lag),
      n_expected   = n(),
      n_valid      = sum(!is.na(raw_clean)),
      
      avg_9AM4hr = calc_reg_avg(date_mt, raw_clean, first(day_mt),  9L, 4L),
      avg_9AM8hr = calc_reg_avg(date_mt, raw_clean, first(day_mt),  9L, 8L),
      avg_4PM4hr = calc_reg_avg(date_mt, raw_clean, first(day_mt), 16L, 4L),
      avg_4PM8hr = calc_reg_avg(date_mt, raw_clean, first(day_mt), 16L, 8L),
      
      n_9am_4 = count_reg_hrs(date_mt, raw_clean, first(day_mt),  9L, 4L),
      n_9am_8 = count_reg_hrs(date_mt, raw_clean, first(day_mt),  9L, 8L),
      n_4pm_4 = count_reg_hrs(date_mt, raw_clean, first(day_mt), 16L, 4L),
      n_4pm_8 = count_reg_hrs(date_mt, raw_clean, first(day_mt), 16L, 8L),
      
      avg_24hr = if (day_complete && n_valid >= 18L) round(mean(raw_clean, na.rm = TRUE), 2) else NA_real_,
      max_24hr = if (day_complete && n_valid >= 18L) round(max(raw_clean, na.rm = TRUE), 2)  else NA_real_,
      
      qa_flagged_hrs     = sum(qa_flag, na.rm = TRUE),
      qa_subbed_hrs = sum(qa_subbed, na.rm = TRUE),
      qa_details         = if (any(qa_flag, na.rm = TRUE)) {
        idx <- which(qa_flag)
        paste(
          if_else(
            qa_subbed[idx],
            sprintf(
              "%s: %.1f -> %.1f (substituted PM25; %s)",
              hr_txt[idx], reg2[idx], raw_clean[idx], qa_reason[idx]
            ),
            sprintf(
              "%s: %.1f (%s)",
              hr_txt[idx], raw_clean[idx], qa_reason[idx]
            )
          ),
          collapse = "; "
        )
      } else {
        NA_character_
      },
      qa_flag = any(qa_flag, na.rm = TRUE),
      
      # Data-completeness QA (missing windows, partial windows, <18-hr days)
      completeness_note = build_completeness_note(
        day_complete, n_valid, n_expected,
        n_9am_4, n_9am_8, n_4pm_4, n_4pm_8
      ),
      completeness_flag = !is.na(completeness_note),
      
      instrument_hrs_REG  = sum(parameter == "PM25"     & !is.na(raw_clean), na.rm = TRUE),
      instrument_hrs_REG2 = sum(parameter == "PM25_COL" & !is.na(raw_clean), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    
    transmute(
      date_clean = day_mt,
      date_txt  = sub(" 0", " ", format(day_mt, "%b %d")),
      year_txt  = format(day_mt, "%Y"),
      avg_9AM4hr, avg_9AM8hr, avg_4PM4hr, avg_4PM8hr,
      avg_24hr, max_24hr, n_valid,
      qa_flag, qa_flagged_hrs, qa_subbed_hrs, qa_details,
      completeness_flag, completeness_note,
      instrument_hrs_REG, instrument_hrs_REG2
    ) %>%
    arrange(desc(date_clean)) %>%
    mutate(
      qa_note_display = case_when(
        !is.na(qa_details) & !is.na(completeness_note) ~ paste("QA/QC:", qa_details, "Incomplete", completeness_note, sep = " | "),
        !is.na(qa_details)                             ~ paste("QA/QC:", qa_details),
        !is.na(completeness_note)                      ~ paste("Incomplete:", completeness_note),
        TRUE                                           ~ NA_character_
      )
    )
  
  #----------------------------------------------------------------------------#
  #----------------------------------------------------------------------------#  
  # FIN!
  # 
  
  if (output == "daily") return(aq_daily_avg)
  
  list(hourly = aq_hourly, daily = aq_daily_avg)
  
}

#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------# 