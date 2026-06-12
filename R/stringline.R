get_global_stop_sequences = function(gtfs, patterns, routeid, directionid) {
  # TODO filter by date
  # The way this works is we assign numeric sequences to every stop on each pattern
  # the stop sequence for every pattern. Then we set each stops stop sequence to the maximum
  # stop sequence on any pattern
  pattern_ids = gtfs$trips[patterns, on = "trip_id"][
    route_id == routeid & direction_id == directionid,
    trip_id
  ]
  relevant_stop_times = gtfs$stop_times[trip_id %in% pattern_ids, ]
  setorder(relevant_stop_times, trip_id, stop_sequence)
  relevant_stop_times[, stop_number := rowid(trip_id)]
  relevant_stop_times[, .(stop_number = max(stop_number)), by = stop_id]
}

stringline = function(gtfs, patterns, routeid) {
  global_stop_sequences = get_global_stop_sequences(gtfs, patterns, routeid, 0)
  trips = gtfs$trips[route_id == routeid & direction_id == 0, ]
  stop_times = gtfs$stop_times[trips, on = "trip_id", nomatch = NULL]
  stop_times = gtfs$stops[stop_times, on = "stop_id"]
  stop_times = global_stop_sequences[stop_times, on = "stop_id"]
  stop_times[, time := to_datetime(arrival_time, lubridate::ymd("2026-01-01"))]

  ggplot(stop_times, aes(x = reorder(stop_name, stop_number), y = time, group = trip_id)) +
    geom_line(linewidth = 0.25) +
    geom_text(
      data = stop_times[, .SD[which.min(stop_sequence)], by = "trip_id"],
      aes(label = trip_id),
      hjust = 0,
      size = 2
    ) +
    scale_y_datetime(labels = \(x) format(x, "%H:%M")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

to_datetime = function(gtfstime, date) {
  spl = stringr::str_split(gtfstime, ":", simplify = T)
  return(
    date +
      lubridate::hours(as.integer(spl[, 1])) +
      lubridate::minutes(as.integer(spl[, 2])) +
      lubridate::seconds(as.integer(spl[, 3]))
  )
}
