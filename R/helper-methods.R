#' @export
.normalizeURL <- function(x) {
    x <- stringi::stri_replace_last_regex(x, "\\?.*", "")
    x <- stringi::stri_replace_last_regex(x, "#.*", "")
    x <- stringi::stri_trim_right(x, pattern = "[^/]")
    return(x)
}

stretch <- function(len, x, default, as.fun) {
    if (is.null(x))
        x <- default
    if (length(x) == 1)
        x <- rep(x, len)
    if (length(x) != len)
        x <- rep(x, length.out = len)
    as.fun(x)
}

rep.data.frame <- function(x, ...) {
    y <- as.data.frame(lapply(x, rep, ...))
    names(y) <- names(x)
    y
}


assert.same.length <- function(...) {
    len <- function(x)
        if (is.data.frame(x) || is.matrix(x))
            dim(x)[1] else length(x)

    l <- list(...)
    if (len(l) == 0) return(character())
    ref.length <- len(l[[1]])
    ref.name <- names(l)[1]

    errors <- mapply(function(key, value) {
        if (!is.null(value) && ref.length != len(value)) {
            paste0("length(", ref.name, ") = ", ref.length,
                   " != length(", key, ") = ", len(value))
        } else {
            as.character(NA)
        }
    }, key = names(l), value = l, USE.NAMES = FALSE)

    errors[!is.na(errors)]
}


#' @export
.get_and_parse <- function(endpoint, query, url_fun, parse_fun) {
    parse_fun(endpoint, .get_json(url_fun(endpoint), query = query))
}

#' @export
.stopifnoquery <- function(query) {
    f <- function(x) {
        is.null(x) || length(x) == 0
    }
    if (all(sapply(query, f)))
        stop("No filter query is given")
}

#' @export
.get_json <- function(url, ...) {
    futile.logger::flog.debug("Requesting %s", url)
    response <- httr::GET(url, httr::add_headers(Accept="application/json"), ...)
    httr::stop_for_status(response)
    jsonlite::fromJSON(httr::content(response, "text"))
}

#' @export
.as.parameter.list <- function(x) {
    if (is.null(x) || length(x) == 0)
        NULL
    else
        paste(x, collapse=",")
}

#' @export
check_length <- function(x, value) {
    if (length(x) != length(value)) {
        stop("incorrect length")
    }
}

#' @export
subset_or_null <- function(x, getter, i) {
    y <- getter(x)
    if (is.null(x)) {
        NULL
    } else {
        y[i]
    }
}


#' @export
.simplify.list <- function(x, path) {
    if (!missing(path)) {
        x <- lapply(x, "[[", path)
    }
    do.call(mapply, c(list(FUN=c, SIMPLIFY=FALSE), x))
}

#' @export
.fetch.resource <- function(x, ...) {
    .fetch.resourceURL(resourceURL(x), ...)
}

#' @export
.fetch.resourceURL <- function(x, ...) {
    args <-  as.list(substitute(list(...)))
    query <- if(is.null(args$query)) list()
    else paste(names(args$query), args$query, collapse="&", sep="=")

    tofetch <- unique(x)

    lapply(tofetch[!(paste(tofetch, query, sep="?") %in% get.cache.keys())],
           function(url) {
               key <- paste(url, query, sep="?")
               value <- .get_json(url, ...)
               set.cache.value(key, value)
           })

    lapply(paste(x, query, sep="?"), function(url) get.cache.value(url))
}

#' @export
.collectionName <- function(x) {
    switch(class(x),
           Service = "services",
           Station = "stations",
           Timeseries = "timeseries",
           Category = "categories",
           Offering = "offerings",
           Feature = "features",
           Procedure = "procedures",
           Phenomenon = "phenomena")
}
