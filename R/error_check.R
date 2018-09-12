#'
#'
error_check <- function(resp,text) {
  if (httr::http_error(resp)) {
    if(httr::http_type(resp) == "application/json") {
      parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                                 simplifyVector = FALSE)
    } else {
      parsed <- NULL
      parsed$error$message = "No error message."
    }
    stop(
      sprintf(
        "%s (HTTP %d). %s.\n %s",
        httr::http_status(httr::status_code(resp))$reason,
        httr::status_code(resp),
        text,
        parsed$error$message
      ),
      call. = FALSE
    )
  }
}
