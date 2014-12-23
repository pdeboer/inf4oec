test.calculateY <- function() {
  checkEquals(calculateY(-0.05946275198388392, 74000, 4266.24364680741), -134)
  checkException(calculateY("x", "x", "x"))
}