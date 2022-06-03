test_that("Always gets same posts", {
  expect_equal(nchar(get_4chan_post(board = "trv", post = 2226503)$comments),
               245)
  expect_error(get_4chan_post(board = "trv", post = 0))
})
