#' Return product info (possibly by SKU)
#' @param sku product SKU
#' @param account target account/profile
#' @importFrom magrittr "%>%"
#' @export
ca_get_products <- function(sku = NULL,
                            account = "US") {

  # prepare filters
  q <- list()
  if (!is.null(sku))
    q <- c(q, "$filter" = paste0("Sku eq '", sku, "'"))

  # call api
  response <- ca_api("Products", query = q, account = account)
  if (length(response) == 0) {
    message("No matching listings were found.")
    return(NULL)
  }

  # parse response; convert fields to appropriate format
  response %>%
    ca_parse_response() %>%
    dplyr::mutate_at(
      c("CreateDateUtc", "UpdateDateUtc", "QuantityUpdateDateUtc", "BlockedDateUtc", "LastSaleDateUtc", "ReceivedDateUtc"),
      ca_parse_datetime
      )
}


