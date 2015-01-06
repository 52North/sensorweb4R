
#' @include generic-methods.R
NULL

#' @export
setClass("HttpResource", slots = list())

setMethod("subresourceURL", "HttpResource", function(x, subresource) {
    paste(resourceURL(x), subresource, sep = "/")
})

setMethod("print", "HttpResource", function(x, ...) {
    cat(.toString.HttpResource(x, ...), "\n")
    invisible(x)
})

setMethod("toString", "HttpResource", function(x, ...) {
    paste("Object of class", class(x), "\n",
          "url:", resourceURL(x))
})
