test.calculateSlope <- function() {
  checkEquals(calculateSlope(4, 23, 3345, 2889),  0.8578269979048189)
  checkException(calculateSlope("x", "x", "x", "x"))
}