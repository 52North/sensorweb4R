library(sensorweb4R)
library(sp)
library(openair)
library(lubridate)

futile.logger::flog.threshold(futile.logger::DEBUG, name = "sensorweb4R")

e <- Endpoint("http://sosrest.irceline.be/api/v1/")

ts <- Timeseries(id = c("ts_fc5991060eb9cfcff239743433e9437a",
                        "ts_9738f2ed188ddb6524b4166a221d0b27",
                        "ts_20130d3498806172d5aaae917211e2ef"),
                 endpoint = e)

time <- strptime(c("2015-01-01T00:00:00Z",
                   "2015-01-31T23:59:59Z"),
                 "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

data <- getData(ts, timespan = new_interval(time[1], time[2]))

times <- unique(sort(do.call(c, lapply(data, time))))
data <- data.frame(lapply(data, function(x) value(x)[match(times, time(x))]))
names(data) <- c("PM10","wd", "ws")
data$wd <- data$wd/10
data$date <- times

pollutionRose(data, pollutant = "PM10")

