#' "Petrify" a day of GTFS-realtime data, i.e. turn it into a GTFS static feed.
#'
#' This takes a data.table with vehicle positions, and a static {gtfstools} GTFS feed object
#' that it matches with, and returns a new static {gtfstools} GTFS feed with the trips that were
#' operated based on the realtime data.
#'
#' The realtime data should be for a single service day.
#' @export
petrify <- function(rt, static, override_calendar = TRUE, ...) {
  checkmate::expect_class(static, "gtfs")
  if (!(checkmate::check_null(static$frequencies) || nrow(static$frequencies) == 0)) {
    cli::cli_warn(c(
      "!" = "{static} contains frequencies, which are not supported.",
      "i" = "They will be ignored"
    ))
  }

  times = interpolate_stop_times(rt, static, ...)

  # now, make it a GTFS feed
  # keep all matched trips
  feed = gtfstools::filter_by_trip_id(static, unique(times$trip_id))

  # convert timestamps to GTFS times
  service_date = lubridate::date(median(times$estimated_time, na.rm = TRUE))
  times[, arrival_time := to_gtfstime(estimated_time, service_date)]
  times[, departure_time := arrival_time]
  times[, c("estimated_time", "prev_stop_sequence", "next_stop_sequence") := NULL]

  # avoid issues with gtfsio not handling columns with multiple classes
  times[, prev_time := as.character(prev_time)]
  times[, next_time := as.character(next_time)]

  feed$stop_times = times

  return(feed)
}

to_gtfstime = function(time, service_date) {
  h = lubridate::hour(time)
  m = lubridate::minute(time)
  s = floor(lubridate::second(time))
  # next-day/overnight service
  h = h + 24 * (lubridate::date(time) - service_date)

  if (any(h < 0)) {
    warning(
      "found times departing before the service day starts, resulting in negative GTFS times, which may cause incorrect results"
    )
  }

  result = sprintf("%02d:%02d:%02d", h, m, s)
  result[is.na(time)] = NA
  result
}
