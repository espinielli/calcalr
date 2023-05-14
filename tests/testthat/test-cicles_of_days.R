test_that("days of week", {
  expect_equal(day_of_week_from_fixed(-214193), SUNDAY)
  expect_equal(name_of_day_of_week(day_of_week_from_fixed(-214193)), "Sunday")
  expect_equal(day_of_week_from_fixed(-61387),  WEDNESDAY)
  expect_equal(name_of_day_of_week(day_of_week_from_fixed(-61387)),  "Wednesday")
  expect_equal(day_of_week_from_fixed(25469),   WEDNESDAY)
  expect_equal(day_of_week_from_fixed(49217),   SUNDAY)
  expect_equal(day_of_week_from_fixed(171307),  WEDNESDAY)
  expect_equal(day_of_week_from_fixed(210155),  MONDAY)
})


test_that("kdays of week", {
  expect_equal(kday_on_or_before(1, -61387),  -61389)
})
