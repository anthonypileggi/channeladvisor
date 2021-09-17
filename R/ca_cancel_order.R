#' Cancel an order in ChannelAdvisor (note: do not notify marketplace)
#' @param orderid ChannelAdvisor OrderID (character/scalar)
#' @param account target Channeladvisor account/profile
#' @export
ca_cancel_order <- function(orderid, account = "US") {

  ca_set_account(account)

  # prepare url
  base_url <- "https://api.channeladvisor.com/v1"
  endpoint <- "Orders"
  endpoint <- paste0(endpoint, "(", orderid, ")/Adjust")
  q <- list(access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"))
  url <- httr::modify_url(base_url, path = file.path("v1", endpoint), query = q)

  # payload
  data <- list(
    PreventSiteProcessing = TRUE
  )

  # get API response
  response <- httr::POST(url, body = data, encode = "json")

  if (httr::http_error(response))
    stop("Could not cancel order!")

  return(response)
}