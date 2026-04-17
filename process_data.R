library(tidyverse)
library(data.table)
library(gtfsrealtime)
library(gtfstools)
library(ggunc)
library(arrow)
devtools::load_all()

DATA_PATH = Sys.getenv("DATA_PATH")

static_stop_times = read_static_stop_times(file.path(DATA_PATH, "gotriangle"))

# we don't have static data going all the way back, just use the first we have
static_stop_times[service_day_start == min(service_day_start), service_day_start := ymd("2026-01-01")]

BASE_PATH = file.path(DATA_PATH, "gotriangle")
TIMEZONE = "America/New_York"
START_DATE = ymd("2026-01-16")
END_DATE = ymd("2026-04-11")

# as.list: https://stackoverflow.com/questions/75874501/looping-iterating-over-a-sequence-of-dates-yields-weird-numbers-whats-their-me
for (start_date in as.list(seq.Date(START_DATE, END_DATE, by = 6))) {
  chunk = read_chunk(BASE_PATH, start_date, start_date + days(6), TIMEZONE)
  estimated_times = interpolate_stop_times(chunk$positions, static_stop_times)
  updates_with_times = match_updates_to_times(chunk$updates, estimated_times)
  write_parquet(updates_with_times, file.path(BASE_PATH, "processed", glue("chunk-{start_date}.parquet")))
}
