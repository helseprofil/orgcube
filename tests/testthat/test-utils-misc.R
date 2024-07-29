test_that("fileaccess", {
  file <- system.file(package = "orgcube")
  expect_error(is_fileaccess("nofile"))
  expect_null(is_fileaccess(file.path(file, "R/cube.R")))
})

test_that("is_coltype", {
  d <- data.table::data.table(X = 1, Z = 1, Y = 1, Y.f = 1, Y.a = 1, Y.n = 1, ROW = 1, KOBLID=1)
  expect_equal(is_coltype(d),
               list(vals = c("Y", "Y.f", "Y.a", "Y.n"),
                    dims = c("X", "Z")))
})
