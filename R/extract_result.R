#' Extract the raw extraction result
#'
#' This method send GET request to \emph{RawExtractionResults} end point in order
#' to download the csv file.
#'
#' The file will be saved to \code{\link[base]{tempfile}} if the \path{path} was
#' not given.
#'
#' @param path Path to save the extracted csv file to. If ommited, it will save
#'   the file to \code{\link[base]{tempfile}}.
#' @param overwrite Will only overwrite existing path if \code{TRUE}.
#' @param jobid The extraction job id.
#' @param aws Will retrieving files directly from the Amazon Web Services cloud
#'   if TRUE.
#' @param silence Will not print out extraction note if \code{TRUE}.
#' @return Path that the csv file was saved.
#'
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
