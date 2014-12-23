test.itemDifficulties <- function() {
  tempItems <- data.frame(1:10, 1:10)
  tempPoints <- c(10, 10)
  tempReturn <- c(0.55, 0.55)
  checkEqualsNumeric(itemDifficulties(tempItems, tempPoints), tempReturn)
  checkException(itemDifficulties("x", "x"))
}