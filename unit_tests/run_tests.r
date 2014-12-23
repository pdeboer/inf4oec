library(RUnit)

# set working directory to source file location at this point
# (Session -> Set Working Directory -> To Source File Location)

source("functions/calculateSlope.R")
source("functions/calculateIntercept.R")
source("functions/calculateY.R")
source("functions/round25.R")
source("functions/itemDifficulties.R")
source("functions/itemTotalCorrelations.R")

test.suite <- defineTestSuite("inf4oec", dirs = file.path("tests"), testFileRegexp = "._test.R")

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)