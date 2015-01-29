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

#' @rdname subset-methods
setMethod("[",
          signature(x = "Category",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  Category(endpoint = endpoint(x)[i],
                           id = id(x)[i],
                           label = label(x)[i],
                           service = service(x)[i])
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

#' @rdname subset-methods
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

#' @rdname subset-methods
setMethod("[",
          signature(x = "DomainResource",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  new(class(x),
                      endpoint = endpoint(x)[i],
                      id = id(x)[i],
                      label = label(x)[i],
                      service = service(x)[i],
                      domainId = domainId(x)[i])
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

#' @rdname subset-methods
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

#' @rdname subset-methods
setMethod("[",
          signature(x = "Endpoint",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  Endpoint(url = resourceURL(x)[i],
                           label = label(x)[i])
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

#' @rdname subset-methods
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

#' @rdname subset-methods
setMethod("[",
          signature(x = "ReferenceValue",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  ReferenceValue(endpoint = endpoint(x)[i],
                                 id = id(x)[i],
                                 label = label(x)[i],
                                 time = time(x)[i],
                                 value = value(x)[i])
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

#' @rdname subset-methods
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

#' @rdname subset-methods
setMethod("[",
          signature(x = "Service",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  Service(endpoint = endpoint(x)[i],
                          id = id(x)[i],
                          label = label(x)[i],
                          serviceURL = serviceURL(x)[i],
                          version = version(x)[i],
                          type = type(x)[i],
                          supportsFirstLatest = supportsFirstLatest(x)[i],
                          quantities = quantities(x)[i,])
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

#' @rdname subset-methods
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

#' @rdname subset-methods
setMethod("[",
          signature(x = "Station",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  Station(endpoint = endpoint(x)[i],
                          id = id(x)[i],
                          label = label(x)[i],
                          geometry = geometry(x)[i])
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

#' @rdname subset-methods
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

#' @rdname subset-methods
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

#' @rdname subset-methods
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

#' @rdname subset-methods
setMethod("[",
          signature(x = "Timeseries",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  Timeseries(endpoint = endpoint(x)[i],
                             id = id(x)[i],
                             label = label(x)[i],
                             uom = uom(x)[i],
                             phenomenon = phenomenon(x)[i],
                             service = service(x)[i],
                             feature = feature(x)[i],
                             offering = offering(x)[i],
                             procedure = procedure(x)[i],
                             category = category(x)[i],
                             station = station(x)[i],
                             statusIntervals = statusIntervals(x)[i],
                             firstValue = firstValue(x)[i],
                             lastValue = lastValue(x)[i])
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

#' @rdname subset-methods
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

#' @rdname subset-methods
setMethod("[",
          signature(x = "TVP",
                    i = "ANY",
                    j = "missing"),
          function(x, i, j, ...) {
              if(is.numeric(i) || is.logical(i)) {
                  TVP(time = time(x)[i],
                      value = value(x)[i])
              } else
                  stop("Indexing only supported with numeric or logical values!")
          })

#' @rdname subset-methods
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
