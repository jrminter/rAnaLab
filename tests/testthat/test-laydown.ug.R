context("laydown.ug.per.sq.cm.from.thick.nm")

test_that("laydown ug.per.sq.cm works", {
  inp <- round(laydown.ug.per.sq.cm.from.thick.nm(36, 7.14),3)
  out <- 25.704
  expect_equal(inp, out)
})
