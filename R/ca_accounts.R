
#' List the available ChannelAdvisor accounts/profiles
#' @export
ca_accounts <- function() {

  # # read from environment variables
  df <- Sys.getenv()
  pid <- df[stringr::str_detect(names(df), "CHANNELADVISOR_PROFILE_ID_")]
  token <- df[stringr::str_detect(names(df), "CHANNELADVISOR_REFRESH_TOKEN_")]
  accounts <- names(df[stringr::str_detect(names(df), "CHANNELADVISOR_PROFILE_ID_")])
  accounts <- stringr::str_replace(accounts, "CHANNELADVISOR_PROFILE_ID_", "")
  out <-
    purrr::pmap(
      list(as.character(pid), as.character(token)),
      function(b,c) { list(profile_id = b, refresh_token = c)}
    )
  names(out) <- accounts

  return(out)

  # list(
  #   US =
  #     list(
  #       profile_id = Sys.getenv("CHANNELADVISOR_PROFILE_ID_US"),
  #       refresh_token = Sys.getenv("CHANNELADVISOR_REFRESH_TOKEN_US")
  #     ),
  #   CA =
  #     list(
  #       profile_id = Sys.getenv("CHANNELADVISOR_PROFILE_ID_CA"),
  #       refresh_token = Sys.getenv("CHANNELADVISOR_REFRESH_TOKEN_CA")
  #     )
  # )
}