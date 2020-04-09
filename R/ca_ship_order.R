#' Mark order as shipped and update tracking
#' @param orderid order ID
#' @param tracking tracking number
#' @param carrier shipping carrier code
#' @param class shipping class code
#' @export
ca_ship_order <- function(orderid, tracking, carrier, class) {

  channeladvisor:::ca_refresh_token()

  # prepare url
  base_url <- "https://api.channeladvisor.com/v1"
  endpoint <- "Orders"
  endpoint <- paste0(endpoint, "(", orderid, ")/Ship")
  q <- list(access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"))
  url <- httr::modify_url(base_url, path = file.path("v1", endpoint), query = q)

  # payload
  data <- list(
    Value = list(
      TrackingNumber = tracking,
      ShippingCarrier = carrier,
      ShippingClass = class,
      DeliveryStatus = "Complete"
    )
  )

  # data <- list(
  #   Value = list(
  #     TrackingNumber = "9261290987363900003098",
  #     ShippingCarrier = "AMZN_US",
  #     ShippingClass = "Standard",
  #     DeliveryStatus = "Complete"
  #   )
  # )

  # get API response
  response <- httr::POST(url, body = data, encode = "json")

  if (httr::http_error(response))
    stop("Could not ship order!")

  return(response)
}