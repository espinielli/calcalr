test_that("epoch is 0", {
  expect_equal(epoch(), 0L)
  expect_equal(rd(0), 0L)
})


test_that("clock_from_moment", {
  c <- clock_from_moment(3.5)
  expect_equal(hour(c), 12)
  expect_equal(minute(c), 0)
  expect_equal(seconds(c), 0)

  c <- clock_from_moment(3.75)
  expect_equal(hour(c), 18)
  expect_equal(minute(c), 0)
  expect_equal(seconds(c), 0)

  c <- clock_from_moment(3.8)
  expect_equal(hour(c), 19)
  expect_equal(minute(c), 11)
  expect_equal(seconds(c), 59.9999, tolerance = 1e-2)

})


test_that("time_from_clock", {
  expect_equal(time_from_clock(c(12, 0, 0)), 0.5, tolerance = 1e-2)
  expect_equal(time_from_clock(c(18, 0, 0)), 0.75, tolerance = 1e-2)
  expect_equal(time_from_clock(c(19, 12, 0)), 0.8, tolerance = 1e-2)
})

test_that("weekday: Appendix C", {
  d1 <- read_test_data("dates1.csv")
  expect_equal(name_of_day_of_week(day_of_week_from_fixed(d1$rd)), d1$day)
})


test_that("(modified) julian day: Appendix C", {
  d1 <- read_test_data("dates1.csv")
  expect_equal(jd_from_fixed(d1$rd), d1$jd)
  expect_equal(fixed_from_jd(d1$jd), d1$rd)

  expect_equal(mjd_from_fixed(d1$rd), d1$mjd)
  expect_equal(fixed_from_mjd(d1$mjd), d1$rd)
})
