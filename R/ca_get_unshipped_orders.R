#' Get unshipped orders
#' @importFrom magrittr "%>%"
#' @export
ca_get_unshipped_orders <- function(account = "US") {

  # prepare filters
  q <- list(
    "$orderby" = paste0("CreatedDateUtc desc"),
    "$filter" = "ShippingStatus eq 'Unshipped'"
    )

  # call api
  response <- ca_api("Orders", query = q, account = account)
  if (length(response) == 0) {
    message("No matching listings were found.")
    return(NULL)
  }

  # parse response; convert fields to appropriate format
  response %>%
    purrr::map_df(~.x[c("ID", "SiteName", "CreatedDateUtc", "ShippingStatus")]) %>%
    dplyr::mutate_at("CreatedDateUtc", channeladvisor:::ca_parse_datetime) %>%
    dplyr::arrange(desc(CreatedDateUtc))
}