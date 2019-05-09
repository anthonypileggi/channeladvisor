#' Update product info
#' @param id product ID
#' @param sku product SKU
#' @param data product information (list/tibble)
#' @importFrom magrittr "%>%"
#' @export
ca_update_product <- function(id = NULL, sku = NULL, data = NULL) {

  ca_refresh_token()

  if (is.null(data)) {
    stop("Must provide product information as `data`!")
  } else {
    fields <- c("Weight", "Cost", "BuyItNowPrice", "Brand", "Manufacturer", "MPN", "Title", "Condition", "ASIN", "UPC")
    if (!all(names(data) %in% fields))
      stop(paste0("Fields provided are not appropriate for this API endpoint! Must be one of: {", paste(fields, collapse = ", "), "}"))
  }

  if (!is.null(sku) & length(sku) == 1)
    id <- ca_get_products(sku)$ID
  if (is.null(id))
    stop("Must provide a product ID or SKU for updating!")

  # prepare url
  base_url <- "https://api.channeladvisor.com/v1"
  endpoint <- "Products"
  endpoint <- paste0(endpoint, "(", id, ")")
  q <- list(access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"))
  url <- httr::modify_url(base_url, path = file.path("v1", endpoint), query = q)

  # get API response
  response <- httr::PUT(url, body = as.list(data), encode = "json")

  if (httr::http_error(response))
    stop("Could not update product information!")

  return(response)
}
