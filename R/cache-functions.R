
.CACHE_NAME <- ".sensorweb4R.cache"

#' @export
clear.cache <- function() {
    set.cache(list())
}

#' @export
get.cache <- function() {
    if (exists(.CACHE_NAME, envir = .GlobalEnv)) {
        cache <- get(.CACHE_NAME, envir = .GlobalEnv)
        if (is.list(cache)) cache else list()
    } else list()
}

#' @export
set.cache <- function(cache) {
    if (!is.list(cache)) stop("not a list")
    assign(.CACHE_NAME, cache, envir = .GlobalEnv)
    invisible(NULL)
}

#' @export
set.cache.value <- function(key, value) {
    cache <- get.cache()
    cache[[key]] <- value
    set.cache(cache)
}

#' @export
set.cache.values <- function(...) {
    list <- list(...)
    cache <- get.cache()
    lapply(names(list), function(name) {
        cache[[name]] <<- list[[name]]
    })
    set.cache(cache)
}

#' @export
get.cache.value <- function(key) {
    if (missing(key)) NULL
    else get.cache()[[key]]
}

#' @export
get.cache.keys <- function() {
    n <- names(get.cache())
    if (is.null(n)) character() else n
}
