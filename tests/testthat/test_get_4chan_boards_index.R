test_that("Always get the 10 most recent threads", {
  expect_equal(nrow(get_4chan_board_index(board = "trv", page = 1,
                                          latest_comments = FALSE)),
               10)
  expect_error(get_4chan_board_index(board = "trv", page = 0))
})

test_that("get_4chan_post() works", {
  testthat::expect_type(get_4chan_board_index(board = "trv", page = 1,
                                              latest_comments = FALSE), "list")
})
