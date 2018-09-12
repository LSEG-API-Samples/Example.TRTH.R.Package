#' Download the user package delivery (data file) and save it to disk.
#'
#' The file are referred to by their PackageDeliveryId.
#'
#' @param packageDeliveryId PackageDeliveryId of the data file.
#' Usually from either \code{\link{package_deliveries_by_package_Id}} or \code{\link{package_deliveries_by_date}}
#'
#' @param path Path to content to.
#'
#' @param overwrite Will only overwrite existing path if \code{TRUE}.
#'
#' @param aws Will retrieve file directly from the Amazon Web Services cloud in
#' which they are hosted if \code{TRUE}.
#'
#' @examples
#' \donttest{
#' get_user_package_delivery("")
#' }
#'
#' @family venue by day
#'
#' @export
get_user_package_delivery <- function(package_delivery_id,
                                      path,
                                      overwrite = FALSE,
                                      aws = FALSE) {
  # Build URL
  url <- sprintf("%s/StandardExtractions/UserPackageDeliveries('%s')/$value",
                 getOption("dss_url"),
                 package_delivery_id)

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
                    httr::progress())

  error_check(resp,"Get user package delivery failed")
  aws_redirect(resp,path)
  return(resp)
}
