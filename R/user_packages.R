#' Retrieve the list of all packages (venues)
#'
#' List packages to which you are entitled for all subscriptions
#'
#' TRTH use server-driven paging to ensures that the quantity of data that is
#' returned by a URI does not overwhelm the servers.
#'
#' The TRTH REST API conforms to \href{http://docs.oasis-open.org/odata/odata/v4.0/errata02/os/complete/part1-protocol/odata-v4.0-errata02-os-part1-protocol-complete.html#_Toc406398310}{OData server driven paging specifications.}
#'
#' In many situations it may not even be necessary to retrieve anything other
#' than the first page.
#'
#' @param pages Number of pages returned. Use \code{"all"} to return all pages.
#'
#' @return Return the list of user package Id, user package name and
#'   the corresponding subscription name.
#'
#' @examples
#' \donttest{
#' user_packages()
#' user_packages(3)
#' user_packages("all")
#' }
#'
#' @family venue by day
#'
#' @export
user_packages <- function(pages = 1) {
  if(pages == "all") {
    pages = 99
  } else if(!is.numeric(pages)||pages < 1){
    pages = 1
  }

  # Build URL
  url <- sprintf("%s/StandardExtractions/UserPackages",
                 getOption("dss_url"))

  get_pages(url, pages)
}

#' List all user package deliveries (data files) for one package.
#'
#' Retrieve a list of available files associated with a Venue using the
#' UserPackageID
#'
#' TRTH use server-driven paging to ensures that the quantity of data that is
#' returned by a URI does not overwhelm the servers.
#'
#' The TRTH REST API conforms to \href{http://docs.oasis-open.org/odata/odata/v4.0/errata02/os/complete/part1-protocol/odata-v4.0-errata02-os-part1-protocol-complete.html#_Toc406398310}{OData server driven paging specifications.}
#'
#' In many situations it may not even be necessary to retrieve anything other
#' than the first page.
#'
#' @param package_id User package Id. Usually from \code{\link{user_packages}}
#'
#' @param pages Number of pages returned. Use \code{"all"} to return all pages.
#'
#' @return Return the list of user package delivery Id.
#'
#' @examples
#' \donttest{
#' package_deliveries_by_package_Id("0x04f21a8d13459cb1")
#' package_deliveries_by_package_Id("0x04f21a8d13459cb1", 3)
#' package_deliveries_by_package_Id("0x04f21a8d13459cb1", "all")
#' }
#'
#' @family venue by day
#'
#' @export
package_deliveries_by_package_Id <- function(package_id,
                                             pages = 1) {
  if(pages == "all") {
    pages = 99
  } else if(!is.numeric(pages)||pages < 1){
    pages = 1
  }

  # Build URL
  url <- sprintf("%s/StandardExtractions/UserPackageDeliveryGetUserPackageDeliveriesByPackageId(PackageId='%s')",
                 getOption("dss_url"),
                 package_id)

  get_pages(url, pages)
}

#' List all user package deliveries (data files) between time range.
#'
#' Retrieve a list of available files published between specific points in time
#' for download. This can be particularly useful to retrieve files that were
#' previously missed.
#'
#' TRTH use server-driven paging to ensures that the quantity of data that is
#' returned by a URI does not overwhelm the servers.
#'
#' The TRTH REST API conforms to \href{http://docs.oasis-open.org/odata/odata/v4.0/errata02/os/complete/part1-protocol/odata-v4.0-errata02-os-part1-protocol-complete.html#_Toc406398310}{OData server driven paging specifications.}
#'
#' In many situations it may not even be necessary to retrieve anything other
#' than the first page.
#'
#' @param from_date Start date. Use ISO 8601 format; e.g. 2007-03-01T13:00:00Z
#'
#' @param to_date End date. Use ISO 8601 format; e.g. 2008-05-11T15:30:00Z
#'
#' @param subscription_id SubscriptionId. Default to TRTH Venue by Day.
#'
#' @param pages Number of pages returned. Use \code{"all"} to return all pages.
#'
#' @return Returns the list of deliveries by date range
#'
#' @examples
#' \donttest{
#' package_deliveries_by_date("2007-03-01T13:00:00Z", "2008-05-11T15:30:00Z")
#' }
#'
#' @family venue by day
#'
#' @export
package_deliveries_by_date <- function(from_date,
                                       to_date,
                                       subscription_id = "0x0400dc1d24a00cb4",
                                       pages = 1) {
  if(pages == "all") {
    pages = 99
  } else if(!is.numeric(pages)||pages < 1){
    pages = 1
  }

  # Build URL
  url <- sprintf("%s/StandardExtractions/UserPackageDeliveryGetUserPackageDeliveriesByDateRange(SubscriptionId='%s',FromDate=%s,ToDate=%s)",
                 getOption("dss_url"),
                 subscription_id,
                 from_date,
                 to_date)

  get_pages(url, pages)
}

#' Retrieve pages.
#'
#' The TRTH REST API conforms to \href{http://docs.oasis-open.org/odata/odata/v4.0/errata02/os/complete/part1-protocol/odata-v4.0-errata02-os-part1-protocol-complete.html#_Toc406398310}{OData server driven paging specifications.}
#'
#' @param url URL of the first page.
#'
#' @param pages Number of pages.
#'
#' @return List of parsed JSON.
get_pages <- function(url, pages) {
  value <- vector("list")
  for (i in 1:pages) {
    resp <- get_page(url)
    value <- c(value,resp$value)
    if (!is.null(resp[["@odata.nextlink"]])) {
      # Update next link.
      url <- resp[["@odata.nextlink"]]
    } else {
      break
    }
  }
  return(value)
}

#' Retrieve a single page
#'
#' This function is called by \code{get_pages}
#' The TRTH REST API conforms to \href{http://docs.oasis-open.org/odata/odata/v4.0/errata02/os/complete/part1-protocol/odata-v4.0-errata02-os-part1-protocol-complete.html#_Toc406398310}{OData server driven paging specifications.}
#'
#' @param url Page URL.
#'
#' @return List of parsed JSON.
get_page <- function(url) {
  token <- get_token()
  resp <- httr::GET(url,httr::add_headers(prefer = "respond-async",Authorization = token))
  error_check(resp,"Get Page failed")
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)
  return(parsed)
}
