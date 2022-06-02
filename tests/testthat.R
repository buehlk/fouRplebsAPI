# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(fouRplebsAPI)

#test_check("fouRplebsAPI")
test_that("Always gets same posts", {
  expect_equal(nchar(get_4chan_post(board = "trv", post = 2226503)$comments), 245)
  expect_error(get_4chan_post(board = "trv", post = 0))
})
