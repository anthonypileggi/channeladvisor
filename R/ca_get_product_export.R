
#' Retreive products via file export (API)
#' @export
ca_get_product_export <- function() {

  # prepare url/parameters
  url <- "https://api.channeladvisor.com/v1"
  endpoint <- "ProductExport"
  q <-
    list(
      access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"),
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
        access_token = Sys.getenv("CHANNELADVISOR_ACCESS_TOKEN"),
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

  # file clean-up
  unlink(c(zfile, outfile))

  return(out)
}