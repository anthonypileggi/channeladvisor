#' Make a call to the ChannelAdvisor API
#' @param endpoint API endpoint
#' @param query named list with query to submit
ca_api <- function(endpoint = "Products", query = NULL) {
  url <- "https://api.channeladvisor.com/v1"
  q <- list(access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"))
  if (!is.null(query))
    q <- c(q, query)
  response <- httr::GET(file.path(url, endpoint), query = q)
  x <- httr::content(response)
  out <- x$value
  next_url <- x[["@odata.nextLink"]]
  i <- 1
  while (!is.null(next_url)) {
    i <- i + 1
    message("Requesting page ", i)
    response <- httr::GET(next_url)
    x <- httr::content(response)
    out <- c(x$value, out)
    next_url <- x[[3]]
  }
  return(out)
}