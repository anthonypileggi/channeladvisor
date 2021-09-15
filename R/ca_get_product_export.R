
#' Retreive products via file export (API)
#' @param account target account/profile
#' @export
ca_get_product_export <- function(account = "US") {

  # set account
  ca_set_account(account)
  access_token <- Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN")

  # prepare url/parameters
  url <- "https://api.channeladvisor.com/v1"
  endpoint <- "ProductExport"
  q <-
    list(
      access_token = access_token,
      profileid = Sys.getenv("CHANNELADVISOR_PROFILE_ID")
    )
  base_url <- httr::modify_url(url, path = file.path("v1", endpoint))
  url <- httr::modify_url(base_url, query = q)

  # request product export
  ca_refresh_token()
  response <- httr::POST(url)

  # check status of export
  #  -- once ready, it can be downloaded
  file_url <- httr::content(response)$ResponseFileUrl
  i <- 0
  while (is.null(file_url)) {
    i <- i + 1
    message("Attempt ", i)
    if (i > 1)
      Sys.sleep(5)
    token <- httr::content(response)$Token
    q <-
      list(
        access_token = access_token,
        token = token
      )
    url <- httr::modify_url(base_url, query = q)
    response <- httr::GET(url)
    file_url <- httr::content(response)$ResponseFileUrl
  }

  # retreive file export
  file <- format(Sys.time(), "ca_export_%m%d%y%H%M%S")
  outfile <- paste0(file, ".txt")
  zfile <- paste0(file, ".zip")
  download.file(file_url, zfile)
  tfile <- unzip(zfile, list = TRUE)$Name[1]
  unzip(zfile, exdir = getwd())
  file.rename(tfile, outfile)

  # load file export
  out <- readr::read_tsv(outfile, guess_max = 100000)

  # parse date columns
  out <-
    dplyr::mutate_at(
      out,
      c("CreateDateUtc", "UpdateDateUtc", "LastSaleDateUtc", "ReceivedDateUtc"),
      ca_parse_datetime
    )

  # unnest attributes
  id <- which(stringr::str_detect(names(out), "Attribute[0-9]+Name"))
  out1 <- out[, id + 1]
  names(out1) <- as.character(out[1, id])
  out <- dplyr::bind_cols(
    out[, -(c(id, id + 1))],
    out1
  )

  # file clean-up
  unlink(c(zfile, outfile))

  return(out)
}