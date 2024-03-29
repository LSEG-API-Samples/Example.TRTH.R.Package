intraday_request <- function(identifier,
                             fields = c("Open",
                                        "High",
                                        "Low",
                                        "Last"),
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
    condition = intraday_condition(range_type = "Delta",days_ago = 1)
  }
  stopifnot(inherits(condition,"intraday_condition"))
  stopifnot(is.vector(fields,mode="character"))

  # Build request body
  b <- list(
    ExtractionRequest = list(
      "@odata.type" = "#DataScope.Select.Api.Extractions.ExtractionRequests.TickHistoryIntradaySummariesExtractionRequest",
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

intraday_condition <- function(range_type = c("Range",
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
                               interval = c("OneSecond",
                                            "FiveSeconds",
                                            "OneMinute",
                                            "FiveMinutes",
                                            "TenMinutes",
                                            "FifteenMinutes",
                                            "OneHour"),
                               persistence = TRUE) {
  range_type = match.arg(range_type)

  me <- list(SortBy = match.arg(sort_by),
             MessageTimeStampIn = match.arg(time_stamp_in),
             TimeRangeMode = match.arg(range_mode),
             ReportDateRangeType = range_type,
             DateRangeTimeZone = timezone,
             ExtractBy = match.arg(extract_by),
             DisplaySourceRIC = display_source_ric,
             SummaryInterval = match.arg(interval),
             TimebarPersistence = persistence
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
  class(me) <- c("intraday_condition","list")
  me
}

get_intraday_fields <- function() {
  # Build URL
  url <- sprintf("%s/Extractions/GetValidExtractionFieldNames(ReportTemplateType=DataScope.Select.Api.Extractions.ReportTemplates.ReportTemplateTypes'TickHistoryIntradaySummaries')",
                 getOption("dss_url"))

  token <- get_token()
  resp <- httr::GET(url,
                    httr::add_headers(prefer = "respond-async",
                                      Authorization = token))
  error_check(resp,"Intraday Summaries fields failed")
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)
  return(parsed$value)
}
