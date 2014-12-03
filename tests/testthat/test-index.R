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
context("Index")

test_that("Index contains all elements", {
    check_test_api()
    response <- sensorweb_index(check_test_api())
    expect_equal(length(response), 8)
    expect_true("services" %in% sapply(X = response, FUN = "[[", "id"))
    expect_true("stations" %in% sapply(X = response, FUN = "[[", "id"))
    expect_true("timeseries" %in% sapply(X = response, FUN = "[[", "id"))
    expect_true("categories" %in% sapply(X = response, FUN = "[[", "id"))
    expect_true("offerings" %in% sapply(X = response, FUN = "[[", "id"))
    expect_true("features" %in% sapply(X = response, FUN = "[[", "id"))
    expect_true("procedures" %in% sapply(X = response, FUN = "[[", "id"))
    expect_true("phenomena" %in% sapply(X = response, FUN = "[[", "id"))
})
