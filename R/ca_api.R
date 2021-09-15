#' Make a call to the ChannelAdvisor API
#' @param endpoint API endpoint
#' @param account account/profile
#' @param query named list with query to submit
#' @export
ca_api <- function(endpoint = "Products",
                   account = "US",
                   ...) {

  # set target account
  ca_set_account(account)

  # prepare url
  base_url <- "https://api.channeladvisor.com/v1"
  q <- list(access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"))
  url <- httr::modify_url(base_url, path = file.path("v1", endpoint), query = q)

  # get API response
  response <- httr::GET(url, ...)

  # refresh stale token and re-submit
  if (httr::http_error(response)) {
    ca_refresh_token()
    q <- list(access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"))
    url <- httr::modify_url(base_url, path = file.path("v1", endpoint), query = q)
    response <- httr::GET(url, ...)
    if (httr::http_error(response))
      stop("API call returned an error!")
  }

  # extract response content
  x <- httr::content(response)
  out <- x$value

  # paginate (if required)
  next_url <- x[["@odata.nextLink"]]
  i <- 1
  while (!is.null(next_url)) {
    i <- i + 1
    message("Requesting page ", i)
    response <- httr::GET(next_url)
    x <- httr::content(response)
    out <- c(x$value, out)
    next_url <- x[["@odata.nextLink"]]
  }

  return(out)
}