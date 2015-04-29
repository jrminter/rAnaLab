context("laydown.mg.per.sq.ft.from.thick.nm")

test_that("laydown mg.per.sq works", {
  inp <- round(laydown.mg.per.sq.ft.from.thick.nm(112.6, 8.90), 3)
  out <- 93.099
  expect_equal(inp, out)
})
