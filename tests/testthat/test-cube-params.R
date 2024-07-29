test_that("is_teller", {
  params = list()
  params[["FILENAMES"]] <- list(TELLER = "FILE1", NEVNER = "FILE2")
  expect_true(is_teller("FILE1", params))
  expect_false(is_teller("FILE2", params))
  expect_false(is_teller("FILE3", params))
})

test_that("is_filter", {
  params <- list(FILFILTRE = data.table::data.table(ID = 1, FILVERSJON = "FILENAME"))
  expect_true(is_filter("FILENAME", params))
})
