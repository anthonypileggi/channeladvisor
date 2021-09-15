#' Set the current ChannelAdvisor account profile (i.e., locale)
#' @param account account name
#' @export
ca_set_account <- function(account = NULL) {

  if (is.null(account))
    stop("Must select a ChannelAdvisor account!")

  out <- ca_accounts()[[account]]
  if (is.null(out))
    stop(paste0("Could not find ChannelAdvisor account info for account `", account, "`!"))

  ### set environment variables
  message(paste0("Setting current ChannelAdvisor Profile to `", account, "`."))
  Sys.setenv("CHANNELADVISOR_ACCOUNT" = account)
  Sys.setenv("CHANNELADVISOR_PROFILE_ID" = out$profile_id)
  Sys.setenv("CHANNELADVISOR_REFRESH_TOKEN" = out$refresh_token)

  ### reset/refresh access token
  ca_refresh_token()

  return(invisible(out))
}