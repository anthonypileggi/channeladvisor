#' Refresh an access token
#' @param access_token dev access token
#' @export
ca_refresh_token <- function() {
  url <- httr::modify_url("https://api.channeladvisor.com/", path = "oauth2/token")
  auth <- httr::authenticate(Sys.getenv("CHANNELADVISOR_APP_ID"), Sys.getenv("CHANNELADVISOR_SECRET"))
  query <- list(grant_type = "refresh_token", refresh_token = Sys.getenv("CHANNELADVISOR_REFRESH_TOKEN"))
  response <- httr::POST(url, auth, body = query, encode = "form")
  token <- httr::content(response)$access_token
  Sys.setenv("CHANNELADVISOR_ACCESS_TOKEN" = token)
  message("ChannelAdvisor access token has been refreshed!")
  return(invisible(token))
}