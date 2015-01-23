library(sensorweb4R)
library(sp)

futile.logger::flog.threshold(futile.logger::DEBUG, name = "sensorweb4R")

e <- as.Endpoint("http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/")

# fetch all services
srv <- fetch(services(e))
str(srv)

# find a service by URL
srv <- srv[serviceURL(srv) == "http://sos.irceline.be/sos"]
str(srv)

# get all stations of the SOS
sta <- stations(e, service = srv)
str(sta)
length(sta)
label(sta)


# get all timeseries for a station
ts <- timeseries(e, service = srv)

# fetch all metadata for a single timeseries
ts <- fetch(ts[1])

# get the timeseries data
data <- getData(ts)[[1]]

# plot it
plot(as.data.frame(data),
     type = "l",
     main = label(ts),
     xlab = "Time",
     ylab = paste0(label(phenomenon(ts)), " (", uom(ts), ")"))
