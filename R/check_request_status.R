#' Send \code{\link[httr]{GET}} request to Monitor URL in order to check
#' the status of the job.
#'
#' Use this function to poll the job status.
#'
#' @param location Monitor URL. The location of the report job.
#' If omit, \pkg{RTRTH} will use the location of the latest report job.
#'
#' @return The \code{\link[httr]{response}} object.
#'
#' @family helper
check_request_status <- function(location = NULL) {
  # validate args
  if(is.null(location))
    location <- get("location",envir = cacheEnv)

  token <- get("token",envir = cacheEnv)
  resp <- httr::GET(location,
                    httr::add_headers(prefer = "respond-async,wait=1",
                                Authorization = token))
  error_check(resp,"Check Request Status failed")
  return(resp)
}
