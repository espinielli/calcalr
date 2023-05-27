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

  tol <- 1e-10
  fminusy <- function(x, y) { fx(x) - y}
  p <- function(a, b) {abs(fminusy(0.5 * (a+b), y)) <= tol}
  e <-function(x) { fminusy(x, y) >= 0}

    #  function y = f(x), f(x) = x, y0 = 1.0; solution is x0 = 1.0
  fx <- function(x) {x}
  y <- 1
  x0 <- 1.0
  expect_equal(binary_search(0.0, 3.1, p, e), x0,  tolerance = tol)

  # new function y = f(x), f(x) = x**2 - 4*x + 4, y0 = 0.0; solution x0=2.0
  y <-0.0
  x0 <- 2.0
  fx <- function(x) {x**2 -4 * x + 4.0}
  expect_equal(binary_search(1.5, 2.5, p, e), x0,  tolerance = tol)

})
