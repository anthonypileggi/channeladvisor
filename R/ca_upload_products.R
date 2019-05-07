
#' Upload a product/inventory file
#' @param file full path to file to upload
#' @param type import type
#' @export
ca_upload_products <- function(file, type = c("AddUpdate", "UpdateOnly", "AddOnly")) {

  # prepare url/parameters
  type <- match.arg(type)
  url <- "https://api.channeladvisor.com/v1"
  endpoint <- "ProductUpload"
  q <-
    list(
      access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"),
      profileid = "12024025",
      templatecode = "20160407113959 Inventory Template",
      importtype = type
      )
  url <- httr::modify_url(url, path = file.path("v1", endpoint), query = q)

  # submit file upload via API
  response <- httr::POST(url, body = httr::upload_file(file))

  return(invisible(response))
}