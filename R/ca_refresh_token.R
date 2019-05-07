#' Refresh an access token
#' @param access_token dev access token
ca_refresh_token <- function() {
  url <- "https://api.channeladvisor.com/"
  response <- httr::POST(
    "https://api.channeladvisor.com/oauth2/token",
    httr::authenticate(Sys.getenv("CA_APP_ID"), Sys.getenv("CA_SECRET")),
    query = list(
      client_id = Sys.getenv("CA_APP_ID"),
      grant_type = "soap",
      developer_key = Sys.getenv("CA_DEV_KEY"),
      password = Sys.getenv("CA_DEV_PASSWORD"),
      account_id = Sys.getenv("CA_ACCOUNT_ID")
      #grant_type = "refresh_token",
      #refresh_token = Sys.getenv("CA_REFRESH_TOKEN")
    )
  )

}