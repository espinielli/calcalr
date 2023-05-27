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

  expect_equal(summa(\(x) 1, 1, \(i) i <= 4), 4)
  expect_equal(summa(\(x) 1, 0, \(i) i >= 4), 0)
  expect_equal(summa(\(x) x**2, 1, \(i) i <= 4), 30)

  expect_equal(nexti(0, \(i) i == 3), 3)
  expect_equal(nexti(0, \(i) i == 0), 0)

  expect_equal(final(0, \(i) i == 3), -1)
  expect_equal(final(0, \(i) i < 3), 2)
  expect_equal(final(0, \(i) i < 0), -1)

})
