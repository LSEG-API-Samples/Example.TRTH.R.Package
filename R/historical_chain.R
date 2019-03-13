#' Resolve current and past chain constituents given a Chain RIC.
#'
#' Instruments may be currently active, or inactive 'historical only' instruments.
#'
#' @param chain_rics vector or list of Chain RICs to return constituents for.
#' @param start_date_time,end_date_time The range's start and end datetimes.
#'   Tick History use ISO 8601 Date and time format.
#'   The format is yyyy-mm-ddThh:mm:ss.sssZ
#'
#' @return List of historical Chain instrument
#'   Return instruments may be currently active, or inactive 'historical only'
#'   instruments.
#'
#' @examples
#' historical_chain("0#.DJI","1996-01-01","2018-01-01")
#'
#' historical_chain(c("0#.DJI","0#AVO.L"),"1996-01-01","2018-01-01")
#'
#' @export
historical_chain <- function(chain_rics,
                             start_date_time,
                             end_date_time) {
  # Build URL
  url <- sprintf("%s/Search/HistoricalChainResolution", getOption("dss_url"))

  # Build request body
  b <- list(
    Request=list(
      ChainRics=I(chain_rics),
      Range=list(
        Start=start_date_time,
        End=end_date_time
      )
    )
  )
  b <- jsonlite::toJSON(b,POSIXt = "ISO8601",auto_unbox = TRUE)

  token <- get("token",envir = cacheEnv)
  resp <- httr::POST(url,
                     httr::add_headers(prefer = "respond-async",
                                       Authorization = token),
                     httr::content_type_json(),
                     body = b,
                     encode = "raw")
  error_check(resp,"Historical Chain Resolution failed")
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)
  return(parsed)
}
