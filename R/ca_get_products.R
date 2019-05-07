#' Return product info (possibly by SKU)
#' @param sku product SKU
#' @param access_token dev access token
#' @export
ca_get_products <- function(sku = NULL, access_token = Sys.getenv("CA_ACCESS_TOKEN")) {
  q <- NULL
  if (!is.null(sku))
    q <- list("$filter" = paste0("Sku eq '", sku, "'"))
  x <- ca_api("Products", query = q)
  ca_parse_response(x)
}