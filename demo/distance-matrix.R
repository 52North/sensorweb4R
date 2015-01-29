
require(maps)
require(mapdata)
require(maptools)
require(rgdal)
library(sensorweb4R)

data(worldHiresMapEnv)


endpoint <- example.endpoints()[2]
sta.all <- stations(endpoint)
station <- sample(sta.all, 1)


sta.near <- nearestStations(station, stations = sta.all, n = 5)

distance(sta.near)

worldHigh <- pruneMap(map(database = "worldHires", region = "Belgium", plot = FALSE))
worldHigh_Lines <- map2SpatialLines(worldHigh, proj4string = CRS(proj4string(geometry(station))))
plot(worldHigh_Lines, col = "grey50")
plot(geometry(sta.all), add = TRUE, cex = 1, pch = 1, lwd = 1, col="grey50")
plot(geometry(sta.near), add = TRUE, cex = 1, pch = 1, lwd = 2, col="blue")
plot(geometry(station), add = TRUE, cex = 1, pch = 1, lwd = 2, col="red")

