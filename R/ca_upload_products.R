
#' Upload a product/inventory file
#' @param file full path to file to upload
#' @param data data.frame/tibble to upload
#' @param type import type
#' @param account account/profile
#' @export
ca_upload_products <- function(file = NULL,
                               data = NULL,
                               type = c("AddUpdate", "UpdateOnly", "AddOnly"),
                               account = Sys.getenv("CHANNELADVISOR_ACCOUNT")) {

  # set target account
  ca_set_account(account)

  # input checks
  if (all(is.null(file), is.null(data)))
    stop("Must provide a `file` or some `data`!")
  if (!is.null(data) & !is.data.frame(data))
    stop("`data` must be a data.frame object!")
  if (all(!is.null(file), !is.null(data)))
    stop("Provide only one of `data` or `file`!")

  # prepare url/parameters
  type <- match.arg(type)
  url <- "https://api.channeladvisor.com/v1"
  endpoint <- "ProductUpload"
  q <-
    list(
      access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"),
      profileid = Sys.getenv("CHANNELADVISOR_PROFILE_ID"),
      templatecode = "20160407113959 Inventory Template",
      importtype = type
      )
  url <- httr::modify_url(url, path = file.path("v1", endpoint), query = q)

  # convert `data` to a file (if required)
  is_temp <- FALSE
  if (is.null(file)) {
    is_temp <- TRUE
    file <- tempfile(fileext = ".csv")
    readr::write_csv(data, file, na = "")
  }

  # submit file upload via API
  response <- httr::POST(url, body = httr::upload_file(file))
  if (httr::http_error(response)) {
    message("---> FAILED <---")
  } else {
    message("---> SUCCESS <---")
  }

  # remove file (if temporary)
  if (is_temp)
    unlink(file)

  return(invisible(response))
}