#' Refresh an access token
#' @param app_id channeladvisor dev application id
#' @param app_secret channeladvisor application secret
#' @param refresh_token channeladvisor refresh token
#' @export
ca_refresh_token <- function(app_id = Sys.getenv("CHANNELADVISOR_APP_ID"),
                             secret = Sys.getenv("CHANNELADVISOR_SECRET"),
                             refresh_token = Sys.getenv("CHANNELADVISOR_REFRESH_TOKEN")) {

  url <- httr::modify_url("https://api.channeladvisor.com/", path = "oauth2/token")
  auth <- httr::authenticate(app_id, secret)
  query <- list(grant_type = "refresh_token", refresh_token = refresh_token)
  response <- httr::POST(url, auth, body = query, encode = "form")
  token <- httr::content(response)$access_token
  Sys.setenv("CHANNELADVISOR_ACCESS_TOKEN" = token)
  message("ChannelAdvisor access token has been refreshed!")
  return(invisible(token))
}