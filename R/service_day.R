# get the date of the preceding 3am
get_service_day = function(timestamp) {
  as.Date(ifelse(
    hour(timestamp) >= 3,
    date(timestamp),
    date(timestamp) - 1
  ))
}
