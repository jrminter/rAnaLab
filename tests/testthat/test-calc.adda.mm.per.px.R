context("calc.adda.mm.per.px")

test_that("calc.adda.mm.per.px works", {
  inp <- round(calc.adda.mm.per.px(150.92),2)
  out <- 26504.11
  expect_equal(inp, out)
})
