library(sensorweb4R)
library(sp)
library(openair)
library(lubridate)

futile.logger::flog.threshold(futile.logger::DEBUG, name = "sensorweb4R")

e <- Endpoint("http://sosrest.irceline.be/api/v1/")

ts <- Timeseries(id = c("ts_fc5991060eb9cfcff239743433e9437a",
                        "ts_d19f4b388bf9ca1f24101c727865aa08",
                        "ts_a9aa3b095c2fac814bcb13cd447a5b15"),
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

