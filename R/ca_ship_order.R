#' Mark order as shipped and update tracking
#' @param orderid order ID
#' @param tracking tracking number
#' @param carrier shipping carrier code
#' @param class shipping class code
#' @param date ship date
#' @param account target account/profile
#' @export
ca_ship_order <- function(orderid,
                          tracking,
                          carrier,
                          class,
                          date = NULL,
                          account = "US") {

  ca_set_account(account)

  # prepare url
  base_url <- "https://api.channeladvisor.com/v1"
  endpoint <- "Orders"
  endpoint <- paste0(endpoint, "(", orderid, ")/Ship")
  q <- list(access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"))
  url <- httr::modify_url(base_url, path = file.path("v1", endpoint), query = q)

  # # set ship date/time (in UTC) -- always set to noon
  if (is.null(date) | date == Sys.Date()) {   # no date provided (or today), assume today
    dt <- Sys.time()
  } else {
    dt <- lubridate::as_datetime(paste(date, "12:00:00"), tz = "EST")
  }
  dt <- lubridate::with_tz(dt, tz = "UTC")

  # payload
  data <- list(
    Value = list(
      TrackingNumber = tracking,
      ShippingCarrier = carrier,
      ShippingClass = class,
      DeliveryStatus = "Complete",
      ShippedDateUtc = format(dt, "%Y-%m-%dT%TZ")
    )
  )

  # get API response
  response <- httr::POST(url, body = data, encode = "json")

  if (httr::http_error(response))
    stop("Could not ship order!")

  return(response)
}