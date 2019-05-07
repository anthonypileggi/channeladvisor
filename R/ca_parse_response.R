#' Parse a response object returned from the ChannelAdvisor API
#' @param x response content
ca_parse_response <- function(r) {
  purrr::map_df(
    purrr::map(r, ~replace(.x, lengths(.) == 0, NA)),
    dplyr::as_tibble
  )
}