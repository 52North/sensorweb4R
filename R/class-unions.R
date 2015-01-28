
library(sp)
#' @import sp

setClassUnion("character_or_NULL", c("character", "NULL"))
setClassUnion("numeric_or_NULL", c("numeric", "NULL"))
setClassUnion("logical_or_NULL", c("logical", "NULL"))
setClassUnion("data.frame_or_NULL", c("data.frame", "NULL"))
setClassUnion("SpatialPoints_or_NULL", c("SpatialPoints", "NULL"))
