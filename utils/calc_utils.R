c_spancost <- function(df, monthcap = 12, span = "month") {
  
  df <- df %>%
    mutate(timestamp = force_tz(timestamp, tzone="UTC"))
  
  cat_df <- df %>% filter(event == "new_value")
  if (nrow(cat_df) == 0) { return(tibble(amount = 0)) }

  cat_dates <- df %>% filter(event %in% c("new_category","new_value"))
  
  
  today = force_tz(as.POSIXct(now(tzone = ""), format = "%Y-%m-%d %H:%M:%OS"), tzone="UTC")
  date_limit = today - months(monthcap)
  if (nrow(cat_dates %>% filter(timestamp > date_limit)) > 2) {
    # We need at least 2 dates to create an interval.
    cat_dates = cat_dates %>% filter(timestamp > date_limit)
    cat_df = cat_df %>% filter(timestamp > date_limit)
  }
  
  
  oldest_date = min(cat_dates$timestamp, na.rm=T)
  newest_date = max(cat_dates$timestamp, na.rm=T)
  interval = interval(oldest_date, newest_date)
  if (oldest_date == newest_date) { return( tibble(amount = cat_df$amount))}
  
  if (span == "month") {
  span = interval / months(1)
  }
  if (span == "year") {
  span = interval / years(1)  
  }
  if (span == "week") {
  span = interval / weeks(1)  
  }
  if (span == "day") {
  span = interval %/% days(1)  
  }
  
  period_cost = cat_df %>% filter(timestamp > oldest_date) %>%
    select(amount) %>% summarize(amount = sum(amount))
  cost = period_cost / span
  #if (df %>% slice(1) %>% select(category) == "Forsikringer") { browser() }
  return(cost)
}

c_monthcosts <- function(df) {
  cat_df <- df %>% filter(event == "new_value")
  
  cat_dates <- df %>% filter(event %in% c("new_category","new_value"))
  oldest_date = force_tz(min(cat_dates$timestamp, na.rm=T), tzone="UTC")
  
  cat_df <- cat_df %>% arrange(timestamp) %>% 
    mutate(timestamp = force_tz(timestamp, tzone="UTC"),
           timestamp_floor = floor_date(timestamp, unit="month"),
           timestamp_ceil = ceiling_date(timestamp, unit="month"),
           cover = interval(lag(timestamp_floor, default=floor_date(oldest_date, "month")), timestamp_ceil),
           cover_real = interval(lag(timestamp, default=oldest_date), timestamp),
           date_min = lag(timestamp, default=oldest_date),
           date_max = timestamp,
           day_span = (cover_real / days(1)),
           day_span = ifelse(day_span == 0, 1, day_span),
           day_cost = amount / day_span,
           month_span = (cover / months(1)),
           month_cost = amount / month_span,
           )
  catc_df <- cat_df %>% uncount(ceiling(month_span), .remove=F, .id = "mid")
  
  
  
  catc_df <- catc_df %>% group_by(eventid) %>%
    mutate(mid = mid-1,
           monthprior = floor_date(date_min,"month"),
           midmonths = months(mid),
           month = floor_date(date_min,"month") + months(mid),
           daycount = days_in_month(month),
           daycountmonth = days_in_month(month),
           ismonth = if_else(month == floor_date(date_min,"month"), T, F),
           ismonth = if_else(month == floor_date(date_max,"month"), T, ismonth),
           daycount = if_else(month == floor_date(date_min,"month"),as.integer(daycount-day(date_min)), daycount),
           daycount = if_else(month == floor_date(date_max,"month"), as.integer(day(date_max)), daycount),
           daycount = if_else(floor_date(date_min,"month") == floor_date(date_max,"month"), as.integer(day_span), daycount),
           cost = daycount * day_cost)
  
  if (nrow(catc_df) == 1) { return(catc_df %>% select(month, amount) %>% rename(cost = amount))}
  #if (df %>% slice(1) %>% select(category) == "Forsikringer") { browser() }
  #catc_df %>% select(eventid, date_min, date_max, day_cost, daycount, month, cost_pr_month) %>% view()
  
  #catc_df %>% group_by(eventid) %>% summarize(days = sum(daycount)) %>% view()
  
  mcosts <- catc_df %>% group_by(month) %>%
    summarize(cost = sum(cost))
  
  return(mcosts)
}

