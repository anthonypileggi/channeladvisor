#' Parse datetime output from ChannelAdvisor API
#' @param x datetime as character
ca_parse_datetime <- function(x) {
  x1 <- as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%OSZ"), tz = "UTC")
  if (all(is.na(x1)))
    x1 <- as.POSIXct(x, format = "%m/%d/%Y %H:%M:%S %p", tz = "UTC")
  if (all(is.na(x1)))
    x1 <- as.POSIXct(x, format = "%m/%d/%Y %H:%M %p", tz = "UTC")
  lubridate::with_tz(x1, tzone = Sys.timezone())
}