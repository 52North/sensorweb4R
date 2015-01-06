#' @export
setClass("Cache",
         slots = list(services = "list",
                      stations = "list",
                      timeseries = "list",
                      categories = "list",
                      offerings = "list",
                      features = "list",
                      procedures = "list",
                      phenomena = "list"))

#' @export
Cache <- function() {
  return(new("Cache"))
}
