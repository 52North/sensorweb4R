
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
