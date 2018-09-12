
cacheEnv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.trth <- list(
    dss_url = "https://hosted.datascopeapi.reuters.com/RestApi/v1"
  )
  toset <- !(names(op.trth) %in% names(op))
  if(any(toset)) options(op.trth[toset])

  invisible()
}
