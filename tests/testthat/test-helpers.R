test_that("amod behavious", {
  expect_equal(amod(8, 4), 4)
  expect_equal(amod(-8, 4), 4)
  expect_equal(amod(8, -4), -4)
  expect_equal(amod(8, -4), -4)
})


test_that("mod3 behavious", {
  expect_equal(mod3(7, 9, 9), 7)
  expect_equal(mod3(-7, 9, 9), -7)

  expect_equal(mod3(7, 4, 9), 7)
  expect_equal(mod3(1, 4, 9), 6)

  expect_equal(summa(\(x) 1, 1, \(i) i<=4), 4)
  expect_equal(summa(\(x) 1, 0, \(i) i>=4), 0)
  expect_equal(summa(\(x) x**2, 1, \(i) i<=4), 30)
})