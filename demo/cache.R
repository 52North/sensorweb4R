library(sensorweb4R)
clear.cache()

# sensorweb4R uses an internal cache to limit the number of requests to a service endpoint
# the following demo shows how to explore and reset the cache. direct cache manipulation is not recommended!

# global cache variable
.sensorweb4R.cache

# make a query to fill the cache
e <- as.Endpoint("http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/")
srv <- fetch(services(e))

# correct access using function
get.cache()

# the cache is a map of URLs to R objects representing parsed JSON responses
str(get.cache(), max.level = 2)

# list all cached URLs - it becomes clear how the cache works: although just one timeseries was queried, related station, offering und categories have been fetched.
get.cache.keys()

# get response for a specific URL
get.cache.value(get.cache.keys()[1])

# clearing the cache
clear.cache()
