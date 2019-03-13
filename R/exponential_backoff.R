#' Exponential Backoff with Jitter
#'
#' @inheritParams on_demand
#'
ebwj <- function(resp,
                 path = NULL,
                 overwrite = FALSE,
                 aws = FALSE,
                 silence = FALSE) {
  i = 0
  attempt = getOption("dss_attempt")
  pause_base = getOption("dss_pause_base")
  pause_cap = getOption("dss_pause_cap")
  pause_min = getOption("dss_pause_min")

  stopifnot(is.numeric(attempt), length(attempt) == 1L)
  stopifnot(is.numeric(pause_base), length(pause_base) == 1L)
  stopifnot(is.numeric(pause_cap), length(pause_cap) == 1L)
  stopifnot(is.numeric(pause_min), length(pause_min) == 1L)

  if(!silence) { pb <- txtProgressBar(max = attempt, width = attempt, char = ".",style = 2) }
  while(!async_check(resp,silence) && i < attempt)
  {
    if(!silence) { setTxtProgressBar(pb, i) }
    pause_length <- max(pause_min, stats::runif(1, max = min(pause_cap, pause_base * (2 ^ i))))
    Sys.sleep(pause_length)
    i <- i + 1
    resp <- check_request_status(resp$headers$location)
  }
  if(!silence) { close(pb) }
  if(i < attempt)
  {
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                                 simplifyVector = FALSE)
    extract_result(path,overwrite,parsed$JobId,aws,silence)
  } else if(!silence)
  {
    message("Attempt exceeded.")
    return(resp$headers$location)
  }
}
