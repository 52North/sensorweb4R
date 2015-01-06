#' @include cache-class.R
#' @include httpresource-class.R
NULL

#' @export
setClass("Endpoint",
         contains = "HttpResource",
         slots = c(url = "character",
                   cache = "Cache"))


.normalizeURL <- function(x) {
    x <- stringi::stri_replace_last_regex(x, "\\?.*", "")
    x <- stringi::stri_replace_last_regex(x, "#.*", "")
    x <- stringi::stri_trim_right(x, pattern = "[^/]")
    return(x)
}


#' @export
Endpoint <- function(url, cache = Cache(), ...) {
    return(new("Endpoint", url = .normalizeURL(url), cache = cache, ...))
}

setClassUnion("Endpoint_or_characters", c("Endpoint", "character"))

.stopifnoquery <- function(query) {
    if (all(sapply(query, is.null))) stop("No filter query is given")
}

.get_json <- function(url, ...) {
    futile.logger::flog.debug("Requesting %s", url)
    response <- httr::GET(url, httr::add_headers(Accept="application/json"), ...)
    httr::stop_for_status(response)
    content <- httr::content(response, "text")
    json <- jsonlite::fromJSON(content)
    return(json)
}

.get_and_parse <- function(endpoint, query, url_fun, parse_fun) {
    parse_fun(endpoint, .get_json(url_fun(endpoint), query = query))
}

#' @export
as.character.Endpoint <- function(x, ...) resourceURL(x)

setMethod("as.character", "Endpoint",
          as.character.Endpoint)

setMethod("resourceURL", "Endpoint",
          function(x, ...) x@url)

setMethod("stationsURL", "Endpoint",
          function(x) subresourceURL(x, "stations"))

setMethod("servicesURL", "Endpoint",
          function(x) subresourceURL(x, "services"))

setMethod("timeseriesURL", "Endpoint",
          function(x) subresourceURL(x, "timeseries"))

setMethod("categoriesURL", "Endpoint",
          function(x) subresourceURL(x, "categories"))

setMethod("offeringsURL", "Endpoint",
          function(x) subresourceURL(x, "offerings"))

setMethod("featuresURL", "Endpoint",
          function(x) subresourceURL(x, "features"))

setMethod("proceduresURL", "Endpoint",
          function(x) subresourceURL(x, "procedures"))

setMethod("phenomenaURL", "Endpoint",
          function(x) subresourceURL(x, "phenomena"))
