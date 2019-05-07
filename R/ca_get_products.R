#' Return product info (possibly by SKU)
#' @param sku product SKU
#' @param access_token dev access token
#' @export
ca_get_products <- function(sku = NULL) {

  # prepare filters
  q <- NULL
  if (!is.null(sku))
    q <- list("$filter" = paste0("Sku eq '", sku, "'"))

  # call api; parse response; convert fields to appropriate format
  ca_api("Products", query = q) %>%
    ca_parse_response() %>%
    dplyr::mutate_at(
      c("CreateDateUtc", "UpdateDateUtc", "QuantityUpdateDateUtc"),
      ca_parse_datetime
      )
}

