METERS_PER_DEGREE_LAT = 40000000 / 360

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
#' so exact stop sequence spacing is not
#'
#' @export
interpolate_stop_times = function(positions, static_stop_times, dist_thresh_meters = 2000) {
  positions[, service_day := get_service_day(timestamp)]

  positions[,
    adjusted_stop_sequence := current_stop_sequence +
      fcase(
        current_status == "IN_TRANSIT_TO" , -0.5  ,
        current_status == "INCOMING_AT"   , -0.25 ,
        current_status == "STOPPED_AT"    ,  0.25 ,
        default = NA_real_
      )
  ]

  positions = positions[!is.na(adjusted_stop_sequence), ]

  # we match trips with positions by service days
  # so we first expand stop times so we have a record of each trip
  # for every service day.
  # this is a little inefficient b/c not every trip runs on every day, but
  # the extras get dropped in the next join
  all_service_days = data.table(service_day = unique(positions$service_day))
  gtfs_days = data.table(gtfs_date = unique(static_stop_times$service_day_start))
  all_service_days = gtfs_days[
    all_service_days,
    on = .(gtfs_date == service_day),
    roll = TRUE,
    .(service_day = i.service_day, gtfs_date = x.gtfs_date)
  ]

  expanded_stop_times = static_stop_times[all_service_days, on = .(service_day_start == gtfs_date), allow.cartesian = T]

  times = positions[
    expanded_stop_times,
    on = .(service_day, trip_id, adjusted_stop_sequence == stop_sequence),
    roll = TRUE,
    .(
      trip_id,
      prev_stop_sequence = x.adjusted_stop_sequence,
      stop_sequence = i.stop_sequence,
      stop_id = i.stop_id,
      stop_lat,
      stop_lon,
      service_day,
      prev_time = timestamp,
      prev_lat = latitude,
      prev_lon = longitude,
      bearing
    ),
    nomatch = NULL
  ]

  stopifnot(all(times$stop_sequence > times$prev_stop_sequence))
  stopifnot(!anyDuplicated(times[, .(service_day, trip_id, stop_sequence)]))

  # and now search forward
  times = positions[
    times,
    on = .(service_day, trip_id, adjusted_stop_sequence == stop_sequence),
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
      service_day,
      next_lat = latitude,
      next_lon = longitude,
      next_time = timestamp,
      next_stop_sequence = x.adjusted_stop_sequence,
      bearing
    ),
    nomatch = NULL
  ]

  # interpolate
  # spherical approximation
  times[, dist_from_prev := sqrt((stop_lat - prev_lat)^2 + ((stop_lon - prev_lon) * cos(stop_lat))^2)]
  times[, dist_to_next := sqrt((stop_lat - next_lat)^2 + ((stop_lon - next_lon) * cos(stop_lat))^2)]
  # interpolate based on these distances
  times[, frac := dist_from_prev / (dist_from_prev + dist_to_next)]
  times[, estimated_time := prev_time + (next_time - prev_time) * frac]

  times = times[(dist_from_prev + dist_to_next) * METERS_PER_DEGREE_LAT < dist_thresh_meters, ]

  return(times)
}
