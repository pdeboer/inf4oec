test.calculateIntercept <- function() {
  checkEquals(calculateIntercept(2889, 0.8578269979048189, 3345), 19.568692008380726)
  checkException(calculateIntercept("x", "x", "x"))
}