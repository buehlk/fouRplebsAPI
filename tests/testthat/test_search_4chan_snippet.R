test_that("Always get the 25 most recent posts", {
  expect_equal(nrow(search_4chan_snippet(boards = "adv", text = "kitties",
                                         show_only = "image", cool = 25,
                                         results = "thread")),
               25)
  expect_error(search_4chan_snippet(start_date = 2222, board = "trv",
                                    text = "kitties", show_only = "text"))

})

test_that("search_4chan_snippet() works", {
  testthat::expect_type(search_4chan_snippet(boards = "adv", text = "kitties",
                                             show_only = "image",
                                             result_type = "results_num",
                                             cool = 25)["total_found"],
                        "integer")
  testthat::capture_output(search_4chan_snippet(boards = "adv",
                                                text = "kitties",
                                             show_only = "image", cool = 25)
  )
  testthat::capture_output(search_4chan_snippet(boards = "pol", text = "trump",
                                                cool = 0)
  )
  testthat::capture_output(fouRplebsAPI::search_4chan_snippet(boards = "adv",
                                                              text = "kitties",
                                                              cool = 0,
                                                              show_only =
                                                                "image",
                                                              results =
                                                                "thread")

  )
})
#
#
