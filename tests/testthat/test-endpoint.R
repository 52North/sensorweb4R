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
context("Endpoint")

test_that("label and label<- do work", {
    e <- example.endpoints()[1]
    expect_equal(label(e), "52N Demo")
    label(e) <- "TEST"
    expect_equal(label(e), "TEST")
})

test_that("names and names<- do work", {
    e <- example.endpoints()[1]
    expect_equal(names(e), "52N Demo")
    names(e) <- "TEST"
    expect_equal(names(e), "TEST")
})

test_that("Resource URLs are correctly generated", {
    e <- example.endpoints()[1]
    expect_equal(resourceURL(e), "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1")
    expect_equal(servicesURL(e), "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/services")
    expect_equal(stationsURL(e), "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/stations")
    expect_equal(timeseriesURL(e), "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/timeseries")
    expect_equal(categoriesURL(e), "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/categories")
    expect_equal(offeringsURL(e), "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/offerings")
    expect_equal(featuresURL(e), "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/features")
    expect_equal(proceduresURL(e), "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/procedures")
    expect_equal(phenomenaURL(e), "http://sensorweb.demo.52north.org/sensorwebclient-webapp-stable/api/v1/phenomena")
})
