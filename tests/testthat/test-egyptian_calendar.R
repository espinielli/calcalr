test_that("egyptian calendar: epoch", {
  expect_equal(epoch(), 0L)
  expect_equal(rd(0), 0L)
})

test_that("smoke tests", {
  testvalue <- 710347
  aDate <- egyptian_date(2694, 7, 10)
  expect_equal(egyptian_from_fixed(testvalue), aDate)
  expect_equal(fixed_from_egyptian(aDate), testvalue)
})


test_that("egyptian calendar: Appendix C", {
  d1 <- read_test_data("dates1.csv")
  expect_equal(egyptian_from_fixed(d1$rd),
               egyptian_date(d1$egyptian_year, d1$egyptian_month, d1$egyptian_day))
  expect_equal(egyptian_date(d1$egyptian_year, d1$egyptian_month, d1$egyptian_day),
               egyptian_from_fixed(d1$rd))
})
