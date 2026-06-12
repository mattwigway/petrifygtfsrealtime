METERS_PER_DEGREE_LAT = 40000000 / 360
KMH_TO_MS = 1000 / 3600

#' Interpolate the times vehicles actually passed each stop,
#' based on vehicle positions.
#'
#' This works by modifying current_stop_sequence s.t. current_stop_sequence = x means the exact moment
#' we arrived at the stop
#' IN_TRANSIT_TO means have not arrived at stop yet, so we make the stop sequence smaller -
#' we want to find the last IN_TRANSIT_TO this stop (or, if there isn't one, the last STOPPED_AT
#' from an earlier stop). The spec is unclear on whether this means in transit to the stop or to the next,
#' but the GoTriangle GTFS anyways seems to use it to mean going to the stop.
#' INCOMING_AT is similar but closer to arrival
#' STOPPED_AT means we have arrived at the stop in the past, so add a little
#' We then interpolate an arrival time based on distance from the previous and to the next report,
#' so exact stop sequence spacing is not important, just ordering
#'
#' @export
interpolate_stop_times = function(
  positions,
  static,
  extrapolate_speed_kmh = 25,
  dist_thresh_meters = 2000,
  off_route_max_proportion = 0.7,
  on_route_min_stops = 4
) {
  positions[,
    adjusted_stop_sequence := current_stop_sequence +
      fcase(
        current_status == "IN_TRANSIT_TO" , -0.5  ,
        current_status == "INCOMING_AT"   , -0.25 ,
        current_status == "STOPPED_AT"    ,  0.25 ,
        default = NA_real_
      )
  ]

  n_miss = sum(is.na(positions$adjusted_stop_sequence))

  if (n_miss > 0) {
    cli::cli_warn(
      sprintf(
        "%d position reports (%.2f%%) are missing stop sequences, skipping",
        n_miss,
        n_miss / nrow(positions) * 100
      )
    )
  }

  positions = positions[!is.na(adjusted_stop_sequence), ]

  stop_times = static$stops[, .(stop_id, stop_lat, stop_lon)][static$stop_times, on = .(stop_id)]

  setorder(stop_times, trip_id, stop_sequence)
  setorder(positions, trip_id, timestamp)

  # Find the previous realtime position for each stop
  times = positions[
    stop_times,
    # _not_ including stop_id here - need to roll to next or previous stop if the
    # current stop doesn't match
    on = .(trip_id, adjusted_stop_sequence == stop_sequence),
    roll = TRUE,
    .(
      trip_id,
      prev_stop_sequence = x.adjusted_stop_sequence,
      stop_sequence = i.stop_sequence,
      stop_id = i.stop_id,
      stop_lat,
      stop_lon,
      prev_time = timestamp,
      prev_lat = latitude,
      prev_lon = longitude,
      bearing
    )
  ]

  stopifnot(all(times$stop_sequence > times$prev_stop_sequence, na.rm = T))
  stopifnot(!anyDuplicated(times[, .(trip_id, stop_sequence)]))

  # and the next search forward
  times = positions[
    times,
    # _not_ including stop_id here - need to roll to next or previous stop if the
    # current stop doesn't match
    on = .(trip_id, adjusted_stop_sequence == stop_sequence),
    roll = -Inf,
    .(
      trip_id,
      stop_id = i.stop_id,
      prev_time,
      prev_lat,
      prev_lon,
      stop_lat,
      stop_lon,
      stop_sequence,
      prev_stop_sequence,
      next_lat = latitude,
      next_lon = longitude,
      next_time = timestamp,
      next_stop_sequence = x.adjusted_stop_sequence,
      bearing
    )
  ]

  # interpolate
  # spherical approximation
  times[,
    dist_from_prev := s2_distance(
      s2_geog_point(prev_lon, prev_lat),
      s2_geog_point(stop_lon, stop_lat)
    )
  ]
  times[,
    dist_to_next := s2_distance(
      s2_geog_point(stop_lon, stop_lat),
      s2_geog_point(next_lon, next_lat)
    )
  ]
  # interpolate based on these distances
  times[, frac := dist_from_prev / (dist_from_prev + dist_to_next)]
  times[, estimated_time := prev_time + (next_time - prev_time) * frac]

  # we may lose start and end of trip if the vehicle powered up/down and reached the stop before the
  # first position report we recorded. Allow up to one stop per trip to be extrapolated beyond the
  # last position.
  setorder(times, trip_id, stop_sequence)

  extrapolate_start = times[,
    .I[which.max(ifelse(is.na(prev_lat) & !is.na(next_lat), stop_sequence, -Inf))],
    by = trip_id
  ]$V1

  stopifnot(all(is.na(times[extrapolate_start, estimated_time])))
  times[
    extrapolate_start,
    estimated_time := next_time -
      lubridate::period(dist_to_next / (extrapolate_speed_kmh * KMH_TO_MS), "second")
  ]

  extrapolate_end = times[,
    .I[which.min(ifelse(!is.na(prev_lat) & is.na(next_lat), stop_sequence, Inf))],
    by = trip_id
  ]$V1
  stopifnot(all(is.na(times[extrapolate_end, estimated_time])))
  times[
    extrapolate_end,
    estimated_time := prev_time +
      lubridate::period(dist_to_next / (extrapolate_speed_kmh * KMH_TO_MS), "second")
  ]

  times = times[!is.na(estimated_time), ]

  return(times)
}
