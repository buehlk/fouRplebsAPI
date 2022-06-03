test_that("Always gets same thread", {
  expect_equal(nchar(get_4chan_thread(board = "adv", thread_id = 21738271)$comments[1]),
               27)
  expect_error(get_4chan_post(board = "trv", thread_id = 0))
})

test_that("get_4chan_thread() works", {
  testthat::expect_type(get_4chan_thread(board = "adv", thread_id = 21738271), "list")
})
