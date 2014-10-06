context("/api/v1/timeseries")

test_that("Test that first and last value of a specific timeseries can be requested", {
    response <- GET("http://sosrest.irceline.be/api/v1/timeseries/ts_b78a49c9489501558f15b6fe82f5ca9b")
    response.content <- content(response)

    expect_equal(response.content[["firstValue"]][["value"]], 1.965, info = response[["url"]])
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
