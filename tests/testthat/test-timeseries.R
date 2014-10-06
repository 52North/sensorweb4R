context("/api/v1/timeseries")

test_that("Test that first and last value of a specific timeseries can be requested", {
    endpoint <- new("SensorwebEndpoint", url = sensorweb_api_endpoints()[[2]])

    response.content <- timeseries(endpoint, id = "ts_b78a49c9489501558f15b6fe82f5ca9b")
    expect_equal(response.content[["firstValue"]][["value"]], 1.965, info = toString(endpoint))
    # real time data, cannot test last value.. expect_equal(response.content[["lastValue"]][["value"]], 3.4, info = response[["url"]])
})

# test_that("Test that the search endpoint works", {
#     #
# })
#
# test_that("Test that tall resources for a specific service can be requested", {
#     #
# })
#
# test_that("Test that the 'expanded' parameter works", {
#     #
# })
#
# test_that("Test that requesting a specific 'locale' parameter works", {
#     #
# })
