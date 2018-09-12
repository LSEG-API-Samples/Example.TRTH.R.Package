#' Check response status for asynchronous request.
#'
#' Check if response status return \emph{200 (Okay)} or \emph{202 (accepted)}.
#' If \emph{202 (accepted)}, then return \code{FALSE}.
#'
#' @param resp The \code{\link[httr]{response}} object.
#'
#' @return \code{FALSE} if resp is \emph{202 (accepted)}
#' or \code{TRUE} if resp is \emph{200 (Okay)}.
#'
#' @family helper
async_check <- function(resp, silence = FALSE) {
  if (httr::status_code(resp) == 202) {
    # if(!silence){
    #   message("The request has been accepted but has not yet completed executing asynchronously.")
    # }
    assign("location",resp$headers$location,envir = cacheEnv)
    return(FALSE)
  } else if(httr::status_code(resp) == 200) {
    parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                                 simplifyVector = FALSE)
    if(!silence){
      message(parsed$Notes)
    }
    assign("jobid",parsed$JobId,envir = cacheEnv)
    return(TRUE)
  }
}
