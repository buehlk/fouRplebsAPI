test_that("Always get the 20 most recent threads", {
  expect_equal(nrow(search_4chan(boards = "trv", start_date = "2021-04-20",
                                 end_date = "2022-12-21",
                                 text = "mallorca|menorca")),
               67)

  expect_error(search_4chan(boards = "trv", start_date = "2021/04/20",
                            end_date = "2022/12/21", text = "mallorca|menorca"))
})

test_that("search_4chan() works", {
  testthat::expect_type(search_4chan(boards = "trv", start_date = "2021-04-20",
                                     end_date = "2022-12-21",
                                     text = "mallorca|menorca"), "list")
})
