#' @export
identifier <- function( ... , identifier_type = c("Ric","Isin","Cusip","Sedol")) {
  # validate args
  identifier_type <- match.arg(identifier_type)
  input_array <- list(...)
  identifier_list <- vector("list")

  for (i in seq_along(input_array)) {
    if(is.identifier(input_array[[i]])) {
      x <- input_array[[i]]
      y <- x$InstrumentIdentifiers
    } else if(is.character(input_array[[i]])) {
      y <- list(list(Identifier = input_array[[i]], IdentifierType = identifier_type))
    } else {
      stop("Identifier must be character", call. = FALSE)
    }
    identifier_list <- c(identifier_list,y)
  }

  me <- structure(
    list(
      "@odata.type" = "#DataScope.Select.Api.Extractions.ExtractionRequests.InstrumentIdentifierList",
      InstrumentIdentifiers = I(identifier_list)
    ),
    class = c("identifier","list")
  )
}
is.identifier <- function(x) inherits(x, "identifier")

#' @export
c.identifier <- function(...) {
  Reduce(identifier_combine, list(...))
}

identifier_combine <-function(x,y) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x)
  stopifnot(is.identifier(x), is.identifier(y))
  identifier(x,y)
}
