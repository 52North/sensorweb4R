library(sensorweb4R)
library(sp)
library(lubridate)
library(zoo)
library(openair)

# endpoint is rest-api of IRCEL
endpoint <- example.endpoints()[2]

# available services on this endpoint: only one
srv <- services(endpoint)
label(srv)

## get the names of the phenonoma = pollutants
phe <- phenomena(endpoint)
head(label(phe))

##get the names of the stations
sta <- stations(srv)
head(label(sta))

# filter by category: e.g. only Black Carbon stations
cat <- categories(srv)
head(label(cat))
sta <- stations(srv,category=cat[1])
head(label(sta))

# conversion info to dataframe, e.g.: as.data.frame()

# get spatial info on stations
geom <- sp::geometry(sta)
head(geom)

# get all timeseries in service
# ts <- timeseries(srv)
# or filter by station
# ts <- timeseries(sta[1])
# filter by category
ts <- timeseries(endpoint, category=cat[1])[1:2]

## available info on timeseries

str(ts, max.level=2)
ts <- fetch(ts)    # fetch info
label(station(ts))
label(category(ts))
label(procedure(ts))


#### download data

# length of timeseries

lubridate::duration(lubridate::new_interval(time(firstValue(ts)), time(lastValue(ts))))

# filter: only a part of the series requested

last <- min(time(lastValue(ts)))
time <- lubridate::as.interval(lubridate::weeks(1), last-lubridate::weeks(1))

data <- getData(ts,timespan=time)
str(data)

#### Create a scatterplot of two stations ##
############################################

BC <- as.data.frame(data)
head(BC)
class(BC$time2)
names(BC) <- c("time",label(station(ts))[1],"time2",label(station(ts))[2])

scatterPlot(BC, x=label(station(ts))[1], y=label(station(ts))[2],   linear=TRUE)