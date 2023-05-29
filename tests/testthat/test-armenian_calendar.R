test_that("smoke tests", {
  testvalue = 710347
  aDate = armenian_date(1395, 4, 5)

  expect_equal(armenian_from_fixed(testvalue), aDate)
  expect_equal(fixed_from_armenian(aDate), testvalue)
})


test_that("armenian calendar: Appendix C", {
  d1 <- read_test_data("dates1.csv")
  expect_equal(armenian_from_fixed(d1$rd),
               armenian_date(d1$armenian_year, d1$armenian_month, d1$armenian_day))
  expect_equal(armenian_date(d1$armenian_year, d1$armenian_month, d1$armenian_day),
               armenian_from_fixed(d1$rd))
})
