#' Time & Sales On-Demand request
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
tas_request <- function(identifier,
                        fields = c("Trade - Price",
                                   "Trade - Volume",
                                   "Trade - Exchange Time"),
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
    condition = tas_condition(range_type = "Delta",days_ago = 1)
  }
  stopifnot(inherits(condition,"tas_condition"))
  stopifnot(is.vector(fields,mode="character"))

  # Build request body
  b <- list(
    ExtractionRequest = list(
      "@odata.type" = "#DataScope.Select.Api.Extractions.ExtractionRequests.TickHistoryTimeAndSalesExtractionRequest",
      IdentifierList = I(identifier),
      ContentFieldNames = I(fields),
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

#' Time & Sales Condition
#'
#' Use this function to create condition for \code{\link{tas_request}}.
#'
#' @param range_type Report date range type. Possible values are "Range",
#'   "Delta", "Relative".
#' @param start_date,end_date The query start and end date for the "Range" type.
#'   Tick History use ISO 8601 Date and time format.
#'   The format is yyyy-mm-ddThh:mm:ss.sssZ
#' @param days_ago The "Delta" type will retrieve all data within the number of
#'   days of the extraction execution.
#' @param r_start_days_ago,r_end_days_ago The days portion of the beginning and
#'   the end of the "Relative" query time period. It is a positive integer, from
#'   0 through 31, that represents a number of days preceding the data
#'   extraction.
#' @param r_start_time,r_end_time The time portion of the beginning and the end
#'   of the relative query time period. It is a character string representing
#'   hours, minutes, seconds, and (optionally) milliseconds, in the form:
#'   \code{hh:mm:ss:mmm}
#'   The range of values (including optional milliseconds) is 00:00:00.000
#'   through 23:59:59.999
#' @param timezone Specified a time zone for the request.
#'   Accept a .NET time zone ID
#' @param apply_corrections Apply corrections and cancellations to the result.
#' @param display_source_ric Include Current RIC in results.
#' @param extract_by Extract by "Entity" or "Ric".
#' @param time_stamp_in Specify the data time stamp.
#' @param sort_by Sort the data by Ric or by Timestamp
#' @param range_mode Set to "Inclusive" to extract all relevant data between
#'   start and end datetimes (recommended). Set to "Window" to limit extracted
#'   data to a specific time window occurring daily between start and end
#'   datetimes.
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
tas_condition <- function(range_type = c("Range",
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
                          apply_corrections = TRUE,
                          display_source_ric = TRUE,
                          extract_by = c("Entity",
                                         "Ric"),
                          time_stamp_in = c("GmtUtc",
                                            "LocalExchangeTime"),
                          sort_by = c("SingleByRic",
                                      "SingleByTimestamp"),
                          range_mode = c("Inclusive",
                                         "Window")
                          ) {
  range_type = match.arg(range_type)

  me <- list(SortBy = match.arg(sort_by),
             MessageTimeStampIn = match.arg(time_stamp_in),
             TimeRangeMode = match.arg(range_mode),
             ApplyCorrectionsAndCancellations = apply_corrections,
             ReportDateRangeType = range_type,
             DateRangeTimeZone = timezone,
             ExtractBy = match.arg(extract_by),
             DisplaySourceRIC = display_source_ric
             )

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
  class(me) <- c("tas_condition","list")
  me
}

#' @rdname tas_request
#'
#' @export
get_tas_fields <- function() {
  # Build URL
  url <- sprintf("%s/Extractions/GetValidExtractionFieldNames(ReportTemplateType=DataScope.Select.Api.Extractions.ReportTemplates.ReportTemplateTypes'TickHistoryTimeAndSales')",
                 getOption("dss_url"))

  token <- get_token()
  resp <- httr::GET(url,
                    httr::add_headers(prefer = "respond-async",
                                      Authorization = token))
  error_check(resp,"Time&Sale fields failed")
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)
  return(parsed$value)
}
