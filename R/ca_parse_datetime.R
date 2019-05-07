#' Parse datetime output from ChannelAdvisor API
#' @param x datetime as character
ca_parse_datetime <- function(x) {
  x <- as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%OSZ"), tz = "UTC")
  lubridate::with_tz(x, tzone = Sys.timezone())
}