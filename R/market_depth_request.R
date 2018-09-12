market_depth_request <- function(identifier,
                                 fields = c("Ask Price",
                                            "Ask Size",
                                            "Bid Price",
                                            "Bid Size"),
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
    condition = market_depth_condition(range_type = "Delta",days_ago = 1)
  }
  stopifnot(inherits(condition,"market_depth_condition"))
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
      "@odata.type" = "#ThomsonReuters.Dss.Api.Extractions.ExtractionRequests.TickHistoryMarketDepthExtractionRequest",
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
  error_check(resp,"Market Depth extraction failed")
  # Exponential Backoff with Jitter
  ebwj(resp,attempt,pause_base,pause_cap,pause_min,path,overwrite,aws,silence)
}

market_depth_condition <- function(view = c("NormalizedLL2",
                                            "RawMarketByOrder",
                                            "RawMarketByPrice",
                                            "RawMarketMaker",
                                            "LegacyLevel2"),
                                   num_levels = 10,
                                   range_type = c("Range",
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
                                                  "Window")
                                   ) {
  # validate args
  stopifnot(is.numeric(num_levels), length(num_levels) == 1L)
  range_type = match.arg(range_type)

  me <- list(View = match.arg(view),
             NumberOfLevels = num_levels,
             SortBy = match.arg(sort_by),
             MessageTimeStampIn = match.arg(time_stamp_in),
             TimeRangeMode = match.arg(range_mode),
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
  class(me) <- c("market_depth_condition","list")
  me
}

get_market_depth_fields <- function() {
  # Build URL
  url <- sprintf("%s/Extractions/GetValidExtractionFieldNames(ReportTemplateType=ThomsonReuters.Dss.Api.Extractions.ReportTemplates.ReportTemplateTypes'TickHistoryMarketDepth')",
                 getOption("dss_url"))

  token <- get_token()
  resp <- httr::GET(url,
                    httr::add_headers(prefer = "respond-async",
                                      Authorization = token))
  error_check(resp,"Market Depth fields failed")
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)
  return(parsed$value)
}
