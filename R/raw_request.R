raw_request <- function(identifier,
                        start_date,
                        end_date,
                        domain = c("MarketPrice",
                                   "MarketByOrder",
                                   "MarketByPrice",
                                   "MarketMaker"),
                        display_source_ric = TRUE,
                        extract_by = c("Ric",
                                       "Entity"),
                        time_stamp_in = c("GmtUtc",
                                          "LocalExchangeTime"),
                        sort_by = c("SingleByRic",
                                    "SingleByTimestamp")
                        ) {
  # validate args
  if(!inherits(identifier,"identifier_list"))
    stop()
  domain <- match.arg(domain)
  extract_by <- match.arg(extract_by)
  time_stamp_in <- match.arg(time_stamp_in)
  sort_by <- match.arg(sort_by)

  # Build URL
  url <- sprintf("%s/Extractions/ExtractRaw", getOption("dss_url"))

  # Build request body
  b <- list(
    ExtractionRequest = list(
      "@odata.type" = "#ThomsonReuters.Dss.Api.Extractions.ExtractionRequests.TickHistoryRawExtractionRequest",
      IdentifierList = I(identifier),
      Condition = list(
        DomainCode = domain,
        DisplaySourceRIC = display_source_ric,
        ExtractBy = extract_by,
        MessageTimeStampIn = time_stamp_in,
        SortBy = sort_by,
        ReportDateRangeType = "Range",
        QueryStartDate = start_date,
        QueryEndDate = end_date
      )
    )
  )
  b <- jsonlite::toJSON(b,POSIXt = "ISO8601",auto_unbox = TRUE)

  token <- get("token",envir = cacheEnv)
  resp <- httr::POST(url,
                     httr::add_headers(prefer = "respond-async,wait=10",
                                       Authorization = token),
                     httr::content_type_json(),
                     body = b,
                     encode = "raw")
  error_check(resp,"Time&Sale extraction failed")
  async_check(resp)
}
