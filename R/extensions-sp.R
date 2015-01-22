setMethod("rep",
          signature(x = "SpatialPoints"),
          function(x, ...) {
              coords <- coordinates(x)
              ncoords <- matrix(rep(coords, ...), ncol = dim(coords)[[2]])
              dimnames(ncoords) <- list(NULL, dimnames(coords)[[2]])
              SpatialPoints(ncoords, CRS(proj4string(x)), bbox(x))
          })
