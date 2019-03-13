#' On-demand request
#'
#' An on-demand request send a single HTTP request with the specified
#' instruments, fields, and condition.
#'
#' The function use Exponential Backoff with Jitter in order to find
#' an acceptable polling rate. The approach is outlined in
#' \url{https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter}.
#'
#' @param attempt Maximum number of polling attempt.
#' @param pause_base,pause_cap Each polling will wait between 0 to
#'   \code{pause_base * (2 ^ attempt)} second, up to the maximum of \code{pause_cap}.
#' @param pause_min Minimum time to wait between each polling. Please refrain
#'   from setting this number lower than one!
#' @param path Path to save the extracted csv file to. If ommited, it will save
#'   the file to \code{\link[base]{tempfile}}.
#' @param overwrite Will only overwrite existing path if \code{TRUE}.
#' @param aws Will retrieving files directly from the Amazon Web Services cloud
#'   if TRUE.
#' @param silence Will not print out extraction note if \code{TRUE}.
#' @return Path that the csv file was saved.
#'
#' @family on demands
#'
on_demand <- function(b,
                      path = NULL,
                      overwrite = FALSE,
                      aws = FALSE,
                      silence = FALSE)
{
  # Build URL
  url <- sprintf("%s/Extractions/ExtractRaw", getOption("dss_url"))

  # Make the request
  token <- get("token",envir = cacheEnv)
  resp <- httr::POST(url,
                     httr::add_headers(prefer = "respond-async,wait=1",
                                       Authorization = token),
                     httr::content_type_json(),
                     body = b,
                     encode = "raw")

  # Initial error check
  error_check(resp,"On-demand extraction failed")
  # Exponential Backoff with Jitter
  ebwj(resp,path,overwrite,aws,silence)
}

#'
retry_on_demand <- function(location,
                            path = NULL,
                            overwrite = FALSE,
                            aws = FALSE,
                            silence = FALSE)
{
  # validate args
  if(is.null(location))
    location <- get("location",envir = cacheEnv)

  token <- get("token",envir = cacheEnv)
  resp <- httr::GET(location,
                    httr::add_headers(prefer = "respond-async,wait=1",
                                      Authorization = token))
  error_check(resp,"On-demand extraction failed")
  ebwj(resp,path,overwrite,aws,silence)
}
