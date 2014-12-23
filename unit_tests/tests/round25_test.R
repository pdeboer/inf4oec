test.round25 <- function() {
  checkEquals(round25(758435.89992), 758436)
  checkException(round25("x"))
}