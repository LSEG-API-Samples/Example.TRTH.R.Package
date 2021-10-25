#' Raw On-Demand request
#'
#' \code{\link{tas_request}} send a single on-demand request with the specified
#' instruments, fields, and condition.
#'
#' Time and sales is a display of market trading information, showing a view of
#' every detail of a market's price movement.
#'
#' The function use Exponential Backoff with Jitter in order to find
#' an acceptable polling rate. The approach is outlined in
#' \url{https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter}.
#'
#' @param identifier A list of instrument created from \code{\link{identifier}} function.
#' @param fields A Time and Sales content fields.
#'   Use \code{\link{get_tas_fields}} to request a list of all available Time and Sales fields.
#' @param condition A Time and Sales condition created from \code{\link{tas_condition}} function.
#' @inheritParams on_demand
#' @inherit on_demand return
#'
#' @examples
#' \dontrun{
#' iden <- identifier("IBM.N","AAPL.O")
#' fid <- c("Trade - Price","Trade - Volume","Trade - Exchange Time")
#' cond <- tas_condition("Range","2018-09-18","2018-09-19")
#' a <- tas_request(iden,fid,cond)
#' b <- read.csv(a)
#' }
#'
#' @export
raw_request <- function(identifier,
                        condition,
                        path = NULL,
                        overwrite = FALSE,
                        aws = FALSE,
                        silence = FALSE) {
  # validate args
  if (!is.identifier(identifier))
  {
    if (is.character(identifier))
    {
      identifier<-identifier(identifier)
    } else {
      stop("Invalid Identifier", call. = FALSE)
    }

  }
  if (is.null(condition))
  {
    condition = raw_condition(range_type = "Delta",days_ago = 1)
  }
  stopifnot(inherits(condition,"raw_condition"))

  # Build request body
  b <- list(
    ExtractionRequest = list(
      "@odata.type" = "#DataScope.Select.Api.Extractions.ExtractionRequests.TickHistoryRawExtractionRequest",
      IdentifierList = I(identifier),
      Condition = I(condition)
    )
  )
  b <- jsonlite::toJSON(b,POSIXt = "ISO8601",auto_unbox = TRUE)
  on_demand(b,
            path,
            overwrite,
            aws,
            silence)
}

raw_condition <- function(range_type = c("Range",
                                         "Delta",
                                         "Relative"),
                          start_date = NULL,
                          end_date = NULL,
                          days_ago = NULL,
                          r_start_days_ago = NULL,
                          r_end_days_ago = NULL,
                          r_start_time = NULL,
                          r_end_time = NULL,
                          timezone = "UTC",
                          display_source_ric = TRUE,
                          extract_by = c("Entity",
                                         "Ric"),
                          time_stamp_in = c("GmtUtc",
                                            "LocalExchangeTime"),
                          sort_by = c("SingleByRic",
                                      "SingleByTimestamp"),
                          range_mode = c("Inclusive",
                                         "Window"),
                          domain = c("MarketPrice",
                                     "MarketByOrder",
                                     "MarketByPrice",
                                     "MarketMaker"),
                          fids = NULL) {
  range_type = match.arg(range_type)

  me <- list(SortBy = match.arg(sort_by),
             MessageTimeStampIn = match.arg(time_stamp_in),
             TimeRangeMode = match.arg(range_mode),
             ReportDateRangeType = range_type,
             DateRangeTimeZone = timezone,
             ExtractBy = match.arg(extract_by),
             DisplaySourceRIC = display_source_ric,
             DomainCode = match.arg(domain)
  )

  if (!is.null(fids))
  {
    me[["Fids"]] = fids
  }

  if(range_type == "Range"){
    me[["QueryStartDate"]] = start_date
    me[["QueryEndDate"]] = end_date
  }
  if(range_type == "Delta"){
    me[["DaysAgo"]] = days_ago
  }
  if(range_type == "Relative"){
    me[["RelativeStartDaysAgo"]] = r_start_days_ago
    me[["RelativeEndDaysAgo"]] = r_end_days_ago
    me[["RelativeStartTime"]] = r_start_time
    me[["RelativeEndTime"]] = r_end_time
  }
  class(me) <- c("raw_condition","list")
  me
}
