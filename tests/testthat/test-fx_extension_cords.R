test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("split2 assigns names",{
  ttt <- split2(mtcars, 1:nrow(mtcars), nm = row.names(mtcars))

  expect_equal(names(ttt), row.names(mtcars))
})

test_that("parallel_longer works", {
  expect_equal(suppressWarnings(nrow(parallel_longer(mtcars, c("mpg", "cyl")))), 2*nrow(mtcars))
})
