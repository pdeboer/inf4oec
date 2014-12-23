test.itemTotalCorrelations <- function() {
  tempItems <- data.frame(1:10, 1:10)
  tempReturn <- c(1, 1)
  checkEqualsNumeric(itemTotalCorrelations(tempItems), tempReturn)
  checkException(itemDifficulties("x"))
}