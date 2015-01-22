
#' @include virtual-class-api-resource.R
#' @include class-tvp.R
NULL

#' @export
setClass("ReferenceValue",
         contains = c("TVP", "ApiResource"))

#' @export
is.ReferenceValue <- function(x) is(x, "ReferenceValue")
#' @export
as.ReferenceValue <- function(x) as(x, "ReferenceValue")

setClassUnion("ReferenceValue_or_NULL",
              c("ReferenceValue", "NULL"))

#' @export
ReferenceValue <- function(id = character(),label = NULL,
                           endpoint = NULL, time = NULL, value = NULL) {
    id <- as.character(id)
    len <- length(id)
    label <- stretch(len, label, NA, as.character)
    endpoint <- stretch(len, endpoint, as.character(NA), as.Endpoint)
    time <- stretch(len, time, NA, as.POSIXct)
    value <- stretch(len, value, NA, as.numeric)
    new("ReferenceValue", endpoint = endpoint, id = id,
        label = label, time = time, value = value)
}

setAs("character", "ReferenceValue",
      function(from) ReferenceValue(id = from))

rbind2.ReferenceValue <- function(x, y) {
    x <- as.ReferenceValue(x)
    y <- as.ReferenceValue(y)
    ReferenceValue(endpoint = concat(endpoint(x), endpoint(y)),
                   id = c(id(x), id(y)),
                   label = c(label(x), label(y)),
                   time = c(time(x), time(y)),
                   value = c(value(x), value(y)))
}
setMethod("rbind2",
          signature("ReferenceValue", "ReferenceValue"),
          function(x, y)
              concat.pair.ReferenceValue(x, y))
setMethod("rbind2", signature("ReferenceValue", "ANY"),
          function(x, y)
              concat.pair.ReferenceValue(x, as.ReferenceValue(y)))
setMethod("rbind2",
          signature("ANY", "ReferenceValue"),
          function(x, y)
              concat.pair.ReferenceValue(as.ReferenceValue(x), y))
setMethod("rbind2",
          signature("ANY", "ANY"),
          function(x, y)
              concat.pair.ReferenceValue(as.ReferenceValue(x),
                                         as.ReferenceValue(y)))
setMethod("rep",
          signature(x = "ReferenceValue"),
          function(x, ...)
              ReferenceValue(endpoint = rep(endpoint(x), ...),
                             id = rep(id(x), ...),
                             label = rep(label(x), ...),
                             time = rep(time(x), ...),
                             value = rep(value(x), ...)))


