#' Search for historical instruments given an instrument identifier.
#'
#' Return instruments may be currently active, or inactive 'historical only'
#' instruments.
#'
#' @param identifier Instrument identifier.
#' @param start_date_time,end_date_time The range's start and end datetimes.
#'   Tick History use ISO 8601 Date and time format.
#'   The format is yyyy-mm-ddThh:mm:ss.sssZ
#' @param identifier_type The type of identifier.
#'   Supported types are Ric, Isin, Cusip, Sedol.
#'   Search will look for the identifier in all supported types
#'   when not specified.
#' @param results_by Determines what information is returned for each found RIC.
#'   "Ric": Returns information purely based on the RIC history.
#'   "Entity": Returns information based on the entity associated with
#'     the RIC on the end date of the Range. This will cause RIC rename history
#'     to be returned.
#'   Defaults to searching by RIC when not specified.
#'
#' @return List of historical search result
#'   Return instruments may be currently active, or inactive 'historical only'
#'   instruments.
#'
#' @examples
#' historical_search("TRI.N","1996-01-01","2018-01-01",identifier_type = "Ric",results_by = "Entity")
#'
#' @export
historical_search <- function(identifier,
                              start_date_time,
                              end_date_time,
                              identifier_type=c(NULL,"Ric","Isin","Cusip","Sedol"),
                              results_by=c(NULL,"Ric","Entity")) {

  # validate args
  identifier_type <- match.arg(identifier_type)
  results_by <- match.arg(results_by)

  # Build URL
  url <- sprintf("%s/Search/HistoricalSearch", getOption("dss_url"))

  # Build request body
  b <- list(
    Request=list(
      Identifier=identifier,
      Range=list(
        Start=start_date_time,
        End=end_date_time
      )
    )
  )
  if (!is.null(identifier_type)) {
    b$Request$IdentifierType <- jsonlite::unbox(identifier_type)
  }
  if (!is.null(results_by)) {
    b$Request$ResultsBy <- jsonlite::unbox(results_by)
  }
  b <- jsonlite::toJSON(b,POSIXt = "ISO8601",auto_unbox = TRUE)

  token <- get("token",envir = cacheEnv)
  resp <- httr::POST(url,
                     httr::add_headers(prefer = "respond-async",
                                       Authorization = token),
                     httr::content_type_json(),
                     body = b,
                     encode = "raw")
  error_check(resp,"Historical Search failed")
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)
  return(parsed)
}
