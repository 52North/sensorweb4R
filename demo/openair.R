library(sensorweb4R)
library(sp)
library(openair)

stations.by.phenomena <- function(e, phenomena, ...) {
    sta <- lapply(phenomena, function(x)
        stations(e, phenomenon = x, ...))
    sta <- Reduce(intersect, lapply(sta, id))
    fetch(Station(sta, endpoint = e))
}

timeseries.by.phenomena <- function(e, phenomena, ...) {
    ts <- lapply(phenomena, function(x)
        timeseries(e, phenomenon = x, ...))
    fetch(as.Timeseries(ts))
}

futile.logger::flog.threshold(futile.logger::DEBUG, name = "sensorweb4R")

e <- Endpoint("http://sos2.irceline.be/52n-sos-webapp/api/v1/")
#e <- Endpoint("http://sosrest.irceline.be/api/v1/")

# direction, speed
phe.wind <- Phenomenon(id=c("61102", "61110"),endpoint = e)

# get a station with all phenomenons
station <- stations.by.phenomena(e, phe.wind)[1]

# get a pollutant (lets forget that the only pollutant
# available for the station is temperature ;))
phe.pollutant <- phenomena(station)
phe.pollutant <- sample(phe.pollutant[!id(phe.pollutant) %in% id(phe.wind)], 1)

# fetch the corresponding timeseries
ts <- timeseries.by.phenomena(e, rbind2(phe.wind, phe.pollutant), station = station)

# get the data
time <- strptime(c("2015-01-01T00:00:00Z",
                   "2015-01-31T23:59:59Z"),
                 "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

data <- getData(ts, timespan = lubridate::new_interval(time[1], time[2]))

# filter out measurements with partial data
# and convert the data to a data.frame
times <- unique(sort(do.call(c, lapply(data, time))))
data <- data.frame(lapply(data, function(x) value(x)[match(times, time(x))]))
names(data) <- sapply(ts, function(x) switch(id(phenomenon(x)),
                                             "61102" = "wd",
                                             "61110" = "ws",
                                             id(phenomenon(x))))
data$date <- times

# filter out invalid values
data <- data[data[[id(phe.pollutant)]] != -9999.0,]
data <- data[data[["wd"]] != -9999.0,]
data <- data[data[["ws"]] != -9999.0,]

data$wd <- data$wd/10

# plot it
pollutionRose(data, pollutant = names(data)[!names(data) %in% c("wd", "ws", "date")])

