test_that("Always get the 25 most recent posts", {
  expect_equal(nrow(search_4chan_snippet(boards = "adv", text = "kitties",
                                         show_only = "image", cool = 20)),
               25)
})

test_that("search_4chan_snippet() works", {
  testthat::expect_type(search_4chan_snippet(boards = "adv", text = "kitties",
                                             show_only = "image",
                                             result_type = "results_num",
                                             cool = 20)["total_found"], "list")
})
