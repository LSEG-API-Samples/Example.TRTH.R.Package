
cacheEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.trth <- list(
    dss_url = "https://selectapi.datascope.refinitiv.com/RestApi/v1",
    dss_attempt = 15,
    dss_pause_base = 10,
    dss_pause_cap = 60,
    dss_pause_min = 1
  )
  toset <- !(names(op.trth) %in% names(op))
  if(any(toset)) options(op.trth[toset])

  invisible()
}
