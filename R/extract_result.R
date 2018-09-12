#' The file is saved to \code{\link[base]{tempfile}} if the \path{path} was not given.
extract_result <- function(path = NULL,
                           overwrite = FALSE,
                           jobid = NULL,
                           aws = FALSE,
                           silence = FALSE) {
  # validate args
  if(is.null(jobid))
    jobid <- get("jobid",envir = cacheEnv)
  if(is.null(path))
    path <- tempfile(fileext = ".csv.gz")

  # Build URL
  url <- sprintf("%s/Extractions/RawExtractionResults('%s')/$value",
                 getOption("dss_url"),
                 jobid)

  token <- get("token",envir = cacheEnv)

  # We have to configure httr to not redirect a request when it receives
  # a 302 status code.
  # This is because httr forwards all header fields when it redirects,
  # which break the AWS request.
  resp <- httr::GET(url,
                    httr::add_headers(prefer = "respond-async",
                                      Authorization = token),
                    if(aws){httr::add_headers("X-Direct-Download" = "true")},
                    httr::config(http_content_decoding=0,
                                 followlocation=0),
                    httr::write_disk(path,overwrite),
                    if(!silence){httr::progress()})
  error_check(resp,"Extract Results failed")
  aws_redirect(resp,path,silence)
  invisible(path)
}
