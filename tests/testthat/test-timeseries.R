# Copyright 2014 52°North Initiative for Geospatial Open Source Software GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
context("Timeseries")

test_that("Test that first and last value of a specific timeseries can be requested", {
    endpoint <- example.endpoints()
    endpoint <- endpoint[label(endpoint) == "IRCEL-CELINE"]
    ts <- Timeseries(id = "ts_b78a49c9489501558f15b6fe82f5ca9b", endpoint = endpoint)
    ts <- fetch(ts)
    expect_equal(value(firstValue(ts)), 1.965)
    expect_equal(time(firstValue(ts)), as.POSIXct("2012-05-31 01:00:00 CEST"))
    # real time data, cannot test last value..
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
