#' Download from the Amazon Web Services.
#'
#' If the response is 302 (Redirect), read the response’s Location header field
#' and send the original download request to that URL.
#'
#' We have to configure httr to not auto redirect a request when it receives
#' a 302 status code and perform the rediction manually.
#' This is because httr forwards all header fields when it redirects,
#' which break the AWS request.
#'
#' @param resp The response from \code{\link[httr]{GET}}.
#'
#' @param path Path to content to.
#'
#' @family helper
aws_redirect <- function(resp,
                         path,
                         silence = FALSE) {
  if (httr::status_code(resp) == 302) {
    resp <- httr::GET(resp$headers$location,
                      httr::add_headers(prefer = "respond-async"),
                      httr::config(http_content_decoding=0,
                             followlocation=0),
                      httr::write_disk(path,TRUE),
                      if(!silence){httr::progress()})
    error_check(resp, "AWS Redirect Failed")
  }
}
