# Exponential Backoff with Jitter
ebwj <- function(resp,
                 attempt = 15,
                 pause_base = 10,
                 pause_cap = 60,
                 pause_min = 1,
                 path = NULL,
                 overwrite = FALSE,
                 aws = FALSE,
                 silence = FALSE) {
  i = 0
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
  }
}
