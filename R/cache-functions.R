#' Cache
#'
#' \code{sensorweb4R} cache functions.
#'
#' Most requests to the Timeseries are cached for performance reasons.
#' The parsed JSON responses are saved in a global cache object (with
#' the name \code{sensorweb4R:::CACHE_NAME}). In this list response
#' objects are saved by their respective URL.
#'
#' @param cache A \code{list} object to be used as the cache.
#' @param key The key to be used to insert the \code{value}.
#' @param value The value to be inserted under \code{key}.
#' @param ... Named parameters to be inserted into the cache.
#'
#' @author Christian Autermann \email{c.autermann@@52north.org}
#' @rdname cache
#' @name cache
NULL

CACHE_NAME <- ".sensorweb4R.cache"

#' @export
#' @describeIn cache Clears the cache.
clear.cache <- function() {
    set.cache(list())
}

#' @export
#' @describeIn cache Gets the cache object.
get.cache <- function() {
    if (exists(CACHE_NAME, envir = .GlobalEnv)) {
        cache <- get(CACHE_NAME, envir = .GlobalEnv)
        if (is.list(cache)) cache else list()
    } else list()
}

#' @export
#' @describeIn cache Sets the cache object.
set.cache <- function(cache) {
    if (!is.list(cache))
        stop("not a list")
    assign(CACHE_NAME, cache,
           envir = .GlobalEnv)
    invisible(NULL)
}

#' @export
#' @describeIn cache Sets a cache value.
set.cache.value <- function(key, value) {
    cache <- get.cache()
    cache[[key]] <- value
    set.cache(cache)
}

#' @export
#' @describeIn cache Sets multiple cache values.
set.cache.values <- function(...) {
    list <- list(...)
    cache <- get.cache()
    lapply(names(list), function(name) {
        cache[[name]] <<- list[[name]]
    })
    set.cache(cache)
}

#' @export
#' @describeIn cache Gets a cache value.
get.cache.value <- function(key) {
    if (missing(key)) NULL
    else get.cache()[[key]]
}

#' @export
#' @describeIn cache Gets the current cache keys.
get.cache.keys <- function() {
    n <- names(get.cache())
    if (is.null(n)) character() else n
}
