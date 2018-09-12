#' Create an authentication token.
#'
#' The token remains valid for 24 hours.
#' If a token has expired, you can simply create a new one.
#' Token can be reuse as often as you like within that 24-hour period.
#' Once created, \pkg{RTRTH} will cache the token and use it in every requests.
#'
#' @param username DSS username
#' @param password DSS password
#'
#' @return An authentication token
#'
#' @examples
#' \dontrun{
#' login("dss_username","dss_password")
#' }
#'
#' @family authentication tokens
#'
#' @export
login <- function(username,password) {
  url <- sprintf("%s/Authentication/RequestToken", getOption("dss_url"))
  b <- list(Credentials=list( Username=jsonlite::unbox(username),
                              Password=jsonlite::unbox(password)))
  resp <- httr::POST( url,
                   httr::add_headers(prefer = "respond-async"),
                   httr::content_type_json(),
                   body = b,
                   encode = "json")
  error_check(resp,"Authentication Token request failed")
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)
  token <- paste('Token',parsed$value,sep=" ")
  assign("token",token,envir = cacheEnv)
  invisible(token)
}

#' Set authentication token.
#'
#' \pkg{RTRTH} will cache the token and use it in every requests.
#'
#' @param token authentication token
#'
#' @examples
#' \dontrun{
#' token <- "Token _VAID0wfdjsaw30sef4u3"
#' set_token(token)
#' }
#'
#' @family authentication tokens
#'
#' @export
set_token <- function(token) {
  assign("token",token,envir = cacheEnv)
}

#' Get authentication token.
#' @return An authentication token
#' @family authentication tokens
#' @export
get_token <- function() {
  tryCatch(token <- get("token",envir = cacheEnv),
           error = function(e) {
             stop("Authentication token not found, please create an authentication token with login('dss_username','dss_password')",call. = FALSE)
           })
  return(token)
}
