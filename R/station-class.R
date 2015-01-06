NULL

#' @export
#' @importClassesFrom sp SpatialPointsDataFrame
setClass("Station",
         contains = "Resource",
         slots = list(geometry = "SpatialPoints"))

#' @export
Station <- function(endpoint, id, label = character(), geometry) {
  return(new("Station",
             endpoint = endpoint,
             id = id,
             label = label,
             geometry = geometry))
}

setClassUnion("Station_or_characters", c("Station", "character"))
