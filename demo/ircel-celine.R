library(sensorweb4R)
library(sp)
library(lubridate)

.... <- function(...) { invisible(readline()) }

futile.logger::flog.threshold(futile.logger::DEBUG, name = "sensorweb4R")
....("The logger is set to DEBUG level in this demo. Press <Return> to continue.")

e <- as.Endpoint("http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/")
....("Created a new endpoint based on a URL. Press <Return> to continue.")

# request a random timeseries from the server (can take a little longer, restart demo if there is an error here)
my_timeseries <- random.Timeseries(e)
my_timeseries

str(my_timeseries, max.level = 2)
....("A Timeseries object contains all metadata information. Press <Return> to continue.")


# fetch all services
srv <- fetch(services(e))
names(srv) # same as label(srvs)
....("Fetched all services of the endpoint. Press <Return> to continue.")
str(srv, max.level = 2)

# find a service by URL
srv.ircel <- srv[serviceURL(srv) == "http://sos.irceline.be/sos"]

# find a service id via partial label match
srv.ircel <- subset(srv, grepl(c("IRCEL"), sensorweb4R::label(srv)))
str(srv.ircel)
....("The service list can be subsetted with regular R mechanisms. Press <Return> to continue.")


# get all stations of the SOS for the given service (might take some time!)
sta <- stations(e, service = srv.ircel)
sensorweb4R::label(sta) # same as names(sta)
length(sta)
head(cbind(id(sta), sensorweb4R::label(sta)))
....("The stations have an identifier and a label, which can be accessed using the 'label' or 'names' function. Press <Return> to continue.")


# get all timeseries for a service
ts <- timeseries(e, service = srv.ircel)
str(ts, max.level = 2)
length(ts)
id(ts[1:10])
sensorweb4R::label(ts[1:10])
....("Timeseries can be requested for specific services by object... (Press <Return> to continue)")
ts.2 <- timeseries(e, service = "srv_738111ed219f738cfc85be0c8d87843c")
length(ts.2)
names(ts.2[1:10])
....("... and by character id. Press <Return> to continue.")


# get all timeseries for a station
ts <- timeseries(e, station = sta[1])
str(ts, max.level = 3)

# fetch all metadata for a single timeseries
ts.1 <- fetch(ts[1])
str(ts.1, max.level = 2)

# request first and last values of the timeseries
firstValue(ts.1)
lastValue(ts.1)

# define a timespan
time <- "2013-08-01T00:00:00Z/2013-08-02T00:00:00Z"

# or more conveniently using package lubridate
time <- as.interval(days(1), ymd("2013-08-01"))

# get the timeseries data
data <- getData(ts, timespan = time)[[1]]
....("Data is requested and can be analyzed further in R immediately. Press <Return> to continue.")

str(data)


# coerce to data.frame
data.df <- as.data.frame(data)

# statistical overview
summary(data.df)

# plot it
plot(data.df,
     type = "l",
     main = names(ts),
     xlab = "Time",
     ylab = paste0(names(phenomenon(ts)), " (", uom(ts), ")"))

cat("Demo finished.")
