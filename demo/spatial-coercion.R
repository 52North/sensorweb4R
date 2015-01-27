require(maps)
require(mapdata)
require(maptools)
require(rgdal)
data(worldHiresMapEnv)
library(sensorweb4R)

e <- as.Endpoint("http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/")

# fetch all services
srvs <- fetch(services(e))

# get service of interest and its stations
srv.ircel <- subset(srvs, grepl(c("IRCEL"), names(srvs)))
sta <- stations(e, service = srv.ircel)

# geometry of stations
geometry(sta)

# FIXME: coercion
#as.SpatialPointsDataFrame(sta)

# plot map of stations
worldHigh <- pruneMap(map(database = "worldHires", region = "Belgium", plot = FALSE))
worldHigh_Lines <- map2SpatialLines(worldHigh, proj4string = CRS(proj4string(geometry(sta))))
plot(worldHigh_Lines, col = "grey50")
plot(geometry(sta), add = TRUE, cex = 2, pch = 1, lwd = 2)
title(main = paste(length(sta), "Timeseries stations of", names(srv.ircel)))
