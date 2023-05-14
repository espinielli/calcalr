# use more explicit `mod` rather than `%%`
# mod <- function (e1, e2)  .Primitive("%%")

# Return the same as a % b with b instead of 0.
amod <- function(x, y) {
  y + (magrittr::mod(x, -y))
}

