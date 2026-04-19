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
#' so exact stop sequence spacing is not important, just ordering
#'
#' @export
interpolate_stop_times = function(
  positions,
  static_stop_times,
  dist_thresh_meters = 2000,
  off_route_max_proportion = 0.7,
  on_route_min_stops = 4
) {
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

  setorder(expanded_stop_times, service_day, trip_id, stop_sequence)
  setorder(positions, service_day, trip_id, timestamp)

  # filter out trip IDs/service day combinations where the stops are not a subset of the trips
  # these are probably operators forgetting to switch the headsign
  orig_ntrips = positions[, uniqueN(.SD), .SDcols = c("service_day", "trip_id")]

  # find the ones that match to stops in the GTFS
  positions = expanded_stop_times[
    positions,
    on = .(service_day, trip_id, stop_id),
    .(
      service_day,
      trip_id,
      adjusted_stop_sequence,
      timestamp,
      latitude,
      longitude,
      bearing,
      # record the stop ID _from the stop times_, which will be NA if it didn't match
      matched_stop_id = x.stop_id
    )
  ]

  # filter to just the ones where the alleged stops on the trip match the GTFS
  # TODO should also look at order - but for most agencies that won't be an issue
  # because most stops in each direction are different (pairs on opposite sides of street).
  # it's okay if all the GTFS stops aren't there - we might have missed one in between
  # updates if they're close together or if the GPS signal got lost for a bit.

  # Many of the errors seem to be having the correct trip followed by an incorrect one,
  # so don't just throw out if there's a few stops that don't match. We discard (1) trips
  # where more than off_route_max_prop observations are off route
  positions = positions[,
    if (mean(is.na(matched_stop_id)) <= off_route_max_proportion) .SD,
    by = .(service_day, trip_id)
  ]

  # (2) any position updates where stop_sequence runs backwards (and all subsequent updates)
  # (e.g. t834-b404-sl4 on 2026-01-23 - both directions of the trip are in one trip
  # in the realtime data, and stop-sequence somehow goes back to zero - like the AVL
  # knew it was a new trip but the trip ID didn't get updated.)
  positions = positions[,
    # keep up until the point they stop being monotonic, drop everything after that. cumall will be false
    # if anything before this is false
    .SD[cumall(adjusted_stop_sequence >= shift(adjusted_stop_sequence, type = "lag", n = 1, fill = -Inf)), ],
    by = .(service_day, trip_id)
  ]

  # (3) any individual position updates that are off route
  positions = positions[!is.na(matched_stop_id), ]

  # (4) any trips with fewer than on_route_min_stops after the previous filtering
  positions = positions[, if (.N >= on_route_min_stops) .SD, by = .(service_day, trip_id)]

  new_ntrips = positions[, uniqueN(.SD), .SDcols = c("service_day", "trip_id")]

  n_removed = orig_ntrips - new_ntrips
  pct_removed = round(n_removed / orig_ntrips * 100, 2)

  if (n_removed > 0) {
    cat(
      glue(
        "Removed {n_removed} trips ({pct_removed}%) because stop IDs in GTFS-realtime did not match static GTFS."
      ),
      "(Perhaps the operator forgot to change the headsign.)\n"
    )
  }

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
