test_that("Always get the 20 most recent threads", {
  expect_equal(nrow(get_4chan_board_range(board = "o", page_start = 1, page_stop = 2, latest_comments = FALSE)),
               20)
  expect_error(get_4chan_board_range(board = "o", page_start = 1, page_stop = 0))
})

test_that("get_4chan_board_range() works", {
  testthat::expect_type(get_4chan_board_range(board = "o", page_start = 1, page_stop = 2, latest_comments = FALSE), "list")
})
