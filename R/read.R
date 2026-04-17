#' Read a chunk of updates and positions
#'
#' The unit of analysis is the update. So we read n days of updates.
#' Particularly because the data are stored in UTC days (but also because
#' some agencies run service over midnight even if we converted to local
#' time), we may have updates for trips that run on the next day. So we read
#' n days of updates, but n+1 days of positions. This way, every update that
#' can match to a position will, and there is a many to one relationship between
#' updates and positions anyhow, so we're not duplicating any information.
#' We may read the same position in multiple chunks, but each update will get
#' read exactly once (i.e. once for each time it appears, so an update that
#' appears for 20 minutes will appear in 20 files).
#'
#' Expects that the data are arranged in the format used by the UNC GTFS-realtime
#' archive, i.e. base_path/year/month/day/{vehicle_positions|trip_updates}/*,
#' with static gtfs in base_path/static/year-month-day.zip.
#'
#' @export
read_chunk = function(base_path, start_date, end_date, timezone) {
  cat(glue("Reading updates from {start_date} to {end_date} (inclusive)... "))
  updates = map(
    seq.Date(start_date, end_date, 1),
    function(date) {
      cat(glue("{date}.."))
      map(
        Sys.glob(file.path(
          base_path,
          sprintf("%04d/%02d/%02d", year(date), month(date), day(date)),
          "trip-updates",
          "*"
        )),
        function(filename) {
          one_upd = read_gtfsrt_trip_updates(filename, timezone)

          if (nrow(one_upd) == 0) {
            return(NULL)
          }

          # convert to local time
          one_upd$file_timestamp = with_tz(
            ymd_hms(str_extract(filename, "([0-9]+)[^/]+$", group = 1), tz = "Etc/UTC"),
            timezone
          )

          return(one_upd)
        }
      ) |>
        rbindlist()
    }
  ) |>
    rbindlist()

  cat(glue("read {scales::comma(nrow(updates))} updates\n"))

  pos_end_date = end_date + days(1)
  cat(glue("Reading positions from {start_date} to {pos_end_date} (inclusive)..."))
  positions = map(
    seq.Date(start_date, pos_end_date, 1),
    function(date) {
      cat(glue("{date}.."))
      map(
        Sys.glob(file.path(
          base_path,
          sprintf("%04d/%02d/%02d", year(date), month(date), day(date)),
          "vehicle-positions",
          "*"
        )),
        function(filename) {
          one_pos = read_gtfsrt_positions(filename, timezone)

          if (nrow(one_pos) == 0) {
            return(NULL)
          }

          # convert to local time
          one_pos$file_timestamp = with_tz(
            ymd_hms(str_extract(filename, "([0-9]+)[^/]+$", group = 1), tz = "Etc/UTC"),
            timezone
          )

          return(one_pos)
        }
      ) |>
        rbindlist()
    }
  ) |>
    rbindlist()

  cat(glue(".. read {scales::comma(nrow(positions))} positions\n"))

  positions = positions[, .SD[1], by = c("timestamp", "vehicle_id")]

  cat(glue("After deduplication, {scales::comma(nrow(positions))} positions\n"))

  return(list(
    positions = positions,
    updates = updates
  ))
}

#' Read all of the static GTFS into a big table of stop times,
#' with service_day_start indicating the date the data were retrieved.
read_static_stop_times = function(base_path) {
  lapply(Sys.glob(file.path(DATA_PATH, "gotriangle/static/*.zip")), function(filename) {
    gtfs = read_gtfs(filename)
    gtfs$stop_times$service_day_start = ymd(str_extract(filename, "([0-9]{4}-[0-9]+-[0-9]+)[^/]+$", group = 1))
    gtfs$stops[, .(stop_id, stop_lat, stop_lon)][gtfs$stop_times, on = .(stop_id)]
  }) |>
    rbindlist()
}
