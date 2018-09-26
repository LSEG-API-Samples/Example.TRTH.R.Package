tas_request <- function(identifier,
                        fields = c("Trade - Price",
                                   "Trade - Volume",
                                   "Trade - Exchange Time"),
                        condition,
                        attempt = 15,
                        pause_base = 10,
                        pause_cap = 60,
                        pause_min = 1,
                        path = NULL,
                        overwrite = FALSE,
                        aws = FALSE,
                        silence = FALSE) {
  # validate args
  stopifnot(is.identifier(identifier))
  if (is.null(condition))
  {
    condition = tas_condition(range_type = "Delta",days_ago = 1)
  }
  stopifnot(inherits(condition,"tas_condition"))
  stopifnot(is.vector(fields,mode="character"))
  stopifnot(is.numeric(attempt), length(attempt) == 1L)
  stopifnot(is.numeric(pause_base), length(pause_base) == 1L)
  stopifnot(is.numeric(pause_cap), length(pause_cap) == 1L)
  stopifnot(is.numeric(pause_min), length(pause_min) == 1L)

  # Build URL
  url <- sprintf("%s/Extractions/ExtractRaw", getOption("dss_url"))

  # Build request body
  b <- list(
    ExtractionRequest = list(
      "@odata.type" = "#ThomsonReuters.Dss.Api.Extractions.ExtractionRequests.TickHistoryTimeAndSalesExtractionRequest",
      IdentifierList = I(identifier),
      ContentFieldNames = I(fields),
      Condition = I(condition)
    )
  )
  b <- jsonlite::toJSON(b,POSIXt = "ISO8601",auto_unbox = TRUE)

  # Make the request
  token <- get("token",envir = cacheEnv)
  resp <- httr::POST(url,
                     httr::add_headers(prefer = "respond-async,wait=10",
                                       Authorization = token),
                     httr::content_type_json(),
                     body = b,
                     encode = "raw")

  # Initial error check
  error_check(resp,"Time&Sale extraction failed")
  # Exponential Backoff with Jitter
  ebwj(resp,attempt,pause_base,pause_cap,pause_min,path,overwrite,aws,silence)
}

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

get_tas_fields <- function() {
  # Build URL
  url <- sprintf("%s/Extractions/GetValidExtractionFieldNames(ReportTemplateType=ThomsonReuters.Dss.Api.Extractions.ReportTemplates.ReportTemplateTypes'TickHistoryTimeAndSales')",
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
