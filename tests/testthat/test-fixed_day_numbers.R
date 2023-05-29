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
