#' @include class-category.R
#' @include virtual-class-domain-resource.R
#' @include class-endpoint.R
#' @include class-reference-value.R
#' @include class-service.R
#' @include class-station.R
#' @include class-status-interval.R
#' @include class-timeseries.R
#' @include class-tvp.R
NULL

setMethod("[",
          signature(x = "Category",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  Category(endpoint = endpoint(x),
                           id = subset_or_null(x, id, i),
                           label = subset_or_null(x, label, i),
                           service = subset_or_null(x, service, i))
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

setMethod("[<-",
          signature(x = "Category",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, value){
              if(is.Category(value)){
                  #return(Category())
                  stop("TODO implement Category[i]<-")
              } else {
                  x[i] <- as.Category(value)
              }
          })

setMethod("[",
          signature(x = "DomainResource",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  new(class(x),
                      endpoint = endpoint(x),
                      id = subset_or_null(x, id, i),
                      label = subset_or_null(x, label, i),
                      service = subset_or_null(x, service, i),
                      domainId = subset_or_null(x, domainId, i))
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

setMethod("[<-",
          signature(x = "DomainResource",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, value){
              if(is.DomainResource(value)){
                  #return(DomainResource())
                  stop("TODO implement DomainResource[i]<-")
              } else {
                  x[i] <- as.DomainResource(value)
              }
          })

setMethod("[",
          signature(x = "Endpoint",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  stop("TODO implement Endpoint[i]")
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

setMethod("[<-",
          signature(x = "Endpoint",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, value){
              if(is.Endpoint(value)){
                  #return(Endpoint())
                  stop("TODO implement Endpoint[i]<-")
              } else {
                  x[i] <- as.Endpoint(value)
                  x
              }
          })

setMethod("[",
          signature(x = "ReferenceValue",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  ReferenceValue(endpoint = endpoint(x),
                                 id = subset_or_null(x, id, i),
                                 label = subset_or_null(x, label, i),
                                 time = subset_or_null(x, time, i),
                                 value = subset_or_null(x, value, i))
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

setMethod("[<-",
          signature(x = "ReferenceValue",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, value){
              if(is.ReferenceValue(value)){
                  #return(ReferenceValue())
                  stop("TODO implement ReferenceValue[i]<-")
              } else {
                  x[i] <- as.ReferenceValue(value)
                  x
              }
          })

setMethod("[",
          signature(x = "Service",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  Service(endpoint = endpoint(x),
                          id = subset_or_null(x, id, i),
                          label = subset_or_null(x, label, i),
                          serviceURL = subset_or_null(x, serviceURL, i),
                          version = subset_or_null(x, version, i),
                          type = subset_or_null(x, type, i),
                          supportsFirstLatest = subset_or_null(x, supportsFirstLatest, i),
                          quantities = quantities(x)[i,]);
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

setMethod("[<-",
          signature(x = "Service",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, value){
              if(is.Service(value)){
                  #return(Service())
                  stop("TODO implement Service[i]<-")
              } else {
                  x[i] <- as.Service(value)
                  x
              }
          })

setMethod("[",
          signature(x = "Station",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  Station(endpoint = endpoint(x),
                          id = subset_or_null(x, id, i),
                          label = subset_or_null(x, label, i),
                          geometry = subset_or_null(x, sp::geometry, i))
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

setMethod("[<-",
          signature(x = "Station",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, value){
              if(is.Station(value)){
                  #return(Station())
                  stop("TODO implement Station[i]<-")
              } else {
                  x[i] <- as.Station(value)
                  x
              }
          })

setMethod("[",
          signature(x = "StatusInterval",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  stop("TODO implement StatusInterval[i]")
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

setMethod("[<-",
          signature(x = "StatusInterval",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, value){
              if(is.StatusInterval(value)){
                  #return(StatusInterval())
                  stop("TODO implement StatusInterval[i]<-")
              } else {
                  x[i] <- as.StatusInterval(value)
                  x
              }
          })

setMethod("[",
          signature(x = "Timeseries",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  Timeseries(endpoint = endpoint(x),
                             id = subset_or_null(x, id, i),
                             label = subset_or_null(x, label, i),
                             uom = subset_or_null(x, uom, i),
                             phenomenon = subset_or_null(x, phenomenon, i),
                             service = subset_or_null(x, service, i),
                             feature = subset_or_null(x, feature, i),
                             offering = subset_or_null(x, offering, i),
                             procedure = subset_or_null(x, procedure, i),
                             category = subset_or_null(x, category, i),
                             station = subset_or_null(x, station, i),
                             statusIntervals = subset_or_null(x, statusIntervals, i),
                             firstValue = subset_or_null(x, firstValue, i),
                             lastValue = subset_or_null(x, lastValue, i))
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

setMethod("[<-",
          signature(x = "Timeseries",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, value){
              if(is.Timeseries(value)){
                  #return(Timeseries())
                  stop("TODO implement Timeseries[i]<-")
              } else {
                  x[i] <- as.Timeseries(value)
                  x
              }
          })

setMethod("[",
          signature(x = "TVP",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  TVP(time = subset_or_null(x, time, i),
                      value = subset_or_null(x, value, i))
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

setMethod("[<-",
          signature(x = "TVP",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, value){
              if(is.TVP(value)){
                  #return(TVP())
                  stop("TODO implement TVP[i]<-")
              } else {
                  x[i] <- as.TVP(value)
                  x
              }
          })

