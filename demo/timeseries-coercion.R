library(sensorweb4R)
library(xts)
library(zoo)

futile.logger::flog.threshold(futile.logger::DEBUG, name = "sensorweb4R")

e <- as.Endpoint("http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/")

# fetch all services
srv <- fetch(services(e))

# find a service by URL
srv <- srv[serviceURL(srv) == "http://sos.irceline.be/sos"]

# get all stations of the SOS
sta <- stations(e, service = srv)

# get all timeseries for a station
ts <- timeseries(e, service = srv)

# fetch all metadata for a single timeseries
ts <- fetch(ts[1])

# get the timeseries data
data <- getData(ts)[[1]]

# covert to xts
x <- as.xts(data)

xlab <- "Time"
ylab <- paste0(names(phenomenon(ts)), " (", uom(ts), ")")
main <- names(ts)

# plot the data
plot(x, main = main, xlab = xlab, ylab = ylab)

# or directly plot the timeseries
x <- as.xts(ts)[[1]]

plot(x, main = main, xlab = xlab, ylab = ylab)

# covert to zoo
z <- as.zoo(data)

# plot the data
plot(z, main = main, xlab = xlab, ylab = ylab)

# or directly plot the timeseries
z <- as.zoo(ts)[[1]]
plot(z, main = main, xlab = xlab, ylab = ylab)

# or good ol' ts
t <- as.ts(data)

# plot the data
plot(t, main = main, xlab = xlab, ylab = ylab)

# or directly plot the timeseries
t <- as.ts(ts)[[1]]
plot(t, main = main, xlab = xlab, ylab = ylab)

