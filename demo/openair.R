library(sensorweb4R)
library(sp)
library(openair)

futile.logger::flog.threshold(futile.logger::DEBUG, name = "sensorweb4R")

e <- as.Endpoint("http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/")
# find the right service
srv <- fetch(services(e))
srv <- srv[serviceURL(srv) == "http://sos.irceline.be/sos"]

# find the right phenomena
phe <- phenomena(srv)
phe <- fetch(phe[label(phe) %in% c("61110 - WSP-SCA", "61102 - DD", "81102 - PM10")])

# get a station with all phenomenons
sta <- Reduce(intersect, lapply(phe, function(x) {
    id(stations(srv, phenomenon = x))
}))
sta <- fetch(Station(sta, endpoint = e))

# fetch the corresponding timeseries
ts <- fetch(do.call(function(x, y, ...) {
    x <- rbind2(x, y)
    if (nargs()>2) Recall(x, ...) else x
}, lapply(phe, function(x) timeseries(sta, phenomenon = x))))

data <- getData(ts)
wind.speed <- data[[1]]
wind.direction <- data[[2]]
pollutant.pm10 <- data[[3]]

# strangly we do not have any wind data,
# so fill in some fake values
wind.speed <- TVP(time(pollutant.pm10),
                  runif(length(pollutant.pm10), 0, 100))
wind.direction <- TVP(time(pollutant.pm10),
                      runif(length(pollutant.pm10), 0, 360))

# create a data.frame for openair
df <- data.frame(time = time(pollutant.pm10),
                 pm10 = value(pollutant.pm10),
                 ws = value(wind.speed),
                 wd = value(wind.direction))

# plot it
pollutionRose(df, pollutant = "pm10")
