data <- read.csv(file = "~/Desktop/HS14_IBI_Bernstein_Resultate.csv", header = T, sep = ";")

minItem <- 1
maxItem <- max(data$Fragenummer)

#itemType <- as.factor(c("MC", "MC", "MC", "MC", "SC", "MC", "SC", "SC", "SC", "SC", "MC", "MC", "MC", "SC", "SC", "MC", "SC", "MC", "MC", "SC", "MC", "MC", "MC", "MC", "MC", "MC", "MC", "MC", "MC", "MC", "MC", "MC", "MC"))
itemMaxScores <- c(1,1,2,1,1,2,3,3,1,1,1,2,4,1,2,3,4,1,1,3,2,2,5,8,8,2,2,2,2,10,2,5,2)
aFactors <- c(0.25, -0.25, 0.25, 0.25, 0,
                    -0.25, 0.25, -0.25, 0.25, 0,
                    0.25, -0.25, 0.25, 0.25, 0,
                    -0.25, -0.25, 0.25, 0.25, 0,
                    1, -1, -1, -1, -1,
                    -0.25, 0.25, 0.25, -0.25, 0,
                    -1, -1, -1, 1, -1,
                    -1, -1, -1, 1, -1,
                    -1, -1, 1, -1, -1,
                    -1, -1, -1, -1, 1,
                    0.25, -0.25, -0.25, 0.25, 0,
                    -0.25, -0.25, 0.25, 0.25, 0,
                    0.25, -0.25, 0.25, 0.25, 0,
                    -1, 1, -1, -1, -1,
                    -1, 1, -1, -1, -1,
                    -0.25, 0.25, -0.25, -0.25, 0,
                    1, -1, -1, -1, -1,
                    0.25, 0.25, 0.25, 0.25, 0,
                    0.25, -0.25, 0.25, -0.25, 0,
                    -1, -1, 1, -1, -1,
                    -0.25, 0.25, 0.25, 0.25, 0,
                    0.25, -0.25, 0.25, -0.25, 0,
                    -0.25, 0.25, 0.25, -0.25, 0,
                    0.25, -0.25, 0.25, -0.25, 0,
                    0.25, -0.25, -0.25, 0.25, 0,
                    -0.25, 0.25, -0.25, 0.25, 0,
                    -0.25, 0.25, 0.25, -0.25, 0,
                    0.25, -0.25, -0.25, 0.25, 0,
                    -0.25, 0.25, -0.25, 0.25, 0,
                    0.25, -0.25, -0.25, 0.25, 0,
                    -0.25, -0.25, 0.25, -0.25, 0,
                    0.25, -0.25, -0.25, 0.25, 0,
                    -0.25, 0.25, 0.25, 0.25, 0)

bFactors <- c(0.25, -0.25, 0.25, 0.25, 0,
                     0.25, -0.25, 0.25, -0.25, 0,
                     0.25, 0.25, 0.25, -0.25, 0,
                     -0.25, 0.25, -0.25, 0.25, 0,
                     -1, -1, -1, -1, 1,
                     0.25, -0.25, -0.25, 0.25, 0,
                     1, -1, -1, -1, -1,
                     -1, 1, -1, -1, -1,
                     1, -1, -1, -1, -1,
                     -1, -1, -1, 1, -1,
                     -0.25, 0.25, -0.25, 0.25, 0,
                     0.25, -0.25, 0.25, -0.25, 0,
                     0.25, 0.25, 0.25, -0.25, 0,
                     -1, -1, 1, -1, -1,
                     -1, -1, -1, 1, -1,
                     -0.25, -0.25, 0.25, -0.25, 0,
                     -1, -1, -1, 1, -1,
                     0.25, 0.25, 0.25, 0.25, 0,
                     -0.25, 0.25, 0.25, -0.25, 0,
                     -1, -1, -1, 1, -1,
                     0.25, -0.25, 0.25, 0.25, 0,
                     -0.25, 0.25, -0.25, 0.25, 0,
                     -0.25, -0.25, 0.25, 0.25, 0,
                     -0.25, 0.25, 0.25, -0.25, 0,
                     -0.25, 0.25, 0.25, -0.25, 0,
                     -0.25, 0.25, 0.25, -0.25, 0,
                     -0.25, 0.25, -0.25, 0.25, 0,
                     -0.25, -0.25, 0.25, 0.25, 0,
                     0.25, 0.25, -0.25, -0.25, 0,
                     -0.25, 0.25, -0.25, 0.25, 0,
                     0.25, -0.25, -0.25, -0.25, 0,
                     -0.25, 0.25, -0.25, 0.25, 0,
                     0.25, -0.25, 0.25, 0.25, 0)

pointList <- dcast(data, Barcode + Serie ~ Fragenummer, value.var = "Punkte")
pointList$total = rowSums(pointList[3:35])

dataA <- data[which(data$Serie == "A"),]
subScoresA <- matrix(rep(itemMaxScores, 5), ncol = 5, byrow = F) * matrix(aFactors, ncol = 5, byrow = T)
rownames(dataA) <- NULL
answersA <- dataA[, 4:8]
subScoresA <- answersA * do.call(rbind, replicate(nrow(answersA) / maxItem, subScoresA, simplify = F))


dataB <- data[which(data$Serie == "B"),]
subScoresB <- matrix(rep(itemMaxScores, 5), ncol = 5, byrow = F) * matrix(bFactors, ncol = 5, byrow = T)
rownames(dataB) <- NULL
answersB <- dataB[, 4:8]
subScoresB <- answersB * do.call(rbind, replicate(nrow(answersB) / maxItem, subScoresB, simplify = F))

correctA <- dataA
correctA$A <- subScoresA$A
correctA$A[correctA$A > 0] <- 1
correctA$A[correctA$A < 0] <- -1
correctA$B <- subScoresA$B
correctA$B[correctA$B > 0] <- 1
correctA$B[correctA$B < 0] <- -1
correctA$C <- subScoresA$C
correctA$C[correctA$C > 0] <- 1
correctA$C[correctA$C < 0] <- -1
correctA$D <- subScoresA$D
correctA$D[correctA$D > 0] <- 1
correctA$D[correctA$D < 0] <- -1
correctA$E <- subScoresA$E
correctA$E[correctA$E > 0] <- 1
correctA$E[correctA$E < 0] <- -1

correctB <- dataB
correctB$A <- subScoresB$A
correctB$A[correctB$A > 0] <- 1
correctB$A[correctB$A < 0] <- -1
correctB$B <- subScoresB$B
correctB$B[correctB$B > 0] <- 1
correctB$B[correctB$B < 0] <- -1
correctB$C <- subScoresB$C
correctB$C[correctB$C > 0] <- 1
correctB$C[correctB$C < 0] <- -1
correctB$D <- subScoresB$D
correctB$D[correctB$D > 0] <- 1
correctB$D[correctB$D < 0] <- -1
correctB$E <- subScoresB$E
correctB$E[correctB$E > 0] <- 1
correctB$E[correctB$E < 0] <- -1

n <- 2*5*maxItem
no_points_A <- length(which(pointList$Serie == "A" & pointList$total == 0.00))
no_points_B <- length(which(pointList$Serie == "B" & pointList$total == 0.00))

itemData <- data.frame(serie=character(n), frage=numeric(n), subitem=numeric(n), correct=numeric(n), wrong=numeric(n), no_answer=numeric(n), stringsAsFactors=F)
for (item in minItem:maxItem) {
  tmpA <- correctA[correctA$Fragenummer == item,]
  tmpB <- correctB[correctB$Fragenummer == item,]
  
  for (subitem in 4:8) {
    corrA <- sum(tmpA[tmpA[subitem] == 1, subitem])
    wrongA <- abs(sum(tmpA[tmpA[subitem] == -1, subitem]))
    no_answerA <- nrow(tmpA) - corrA - wrongA - no_points_A
    itemData[(item - 1) * 5 + (subitem - 3),] <- c("A", item, subitem, corrA, wrongA, no_answerA)
    
    corrB <- sum(tmpB[tmpB[subitem] == 1, subitem])
    wrongB <- abs(sum(tmpB[tmpB[subitem] == -1, subitem]))
    no_answerB <- nrow(tmpB) - corrB - wrongB - no_points_B
    itemData[(item - 1) * 5 + (subitem - 3) + 5*maxItem,] <- c("B", item, subitem, corrB, wrongB, no_answerB)
  } 
}

itemData$frage <- as.numeric(itemData$frage)
itemData$subitem <- as.numeric(itemData$subitem)
itemData$correct <- as.numeric(itemData$correct)
itemData$wrong <- as.numeric(itemData$wrong)
itemData$no_answer <- as.numeric(itemData$no_answer)

pdf("test.pdf", width=5.8, height=8.3)
for (page in 1:3) {
  rows <- 6
  if(page == 6) {
    rows = 3
  }

  par(mfrow=c(rows,2), mar = c(2, 2, 2, 2))
  from <- (page-1)*rows*2 + 1
  to <- min(33, page*rows*2)
  
  for (item in from:to) {
    correctCountsA <- as.matrix(dcast(itemData[itemData$serie == "A" & itemData$frage == item,], serie + frage ~ subitem, value.var = "correct")[, 3:7])
    wrongCountsA <- as.matrix(dcast(itemData[itemData$serie == "A" & itemData$frage == item,], serie + frage ~ subitem, value.var = "wrong")[, 3:7])
    noCountsA <- as.matrix(dcast(itemData[itemData$serie == "A" & itemData$frage == item,], serie + frage ~ subitem, value.var = "no_answer")[, 3:7])
  
    correctCountsB <- as.matrix(dcast(itemData[itemData$serie == "B" & itemData$frage == item,], serie + frage ~ subitem, value.var = "correct")[, 3:7])
    wrongCountsB <- as.matrix(dcast(itemData[itemData$serie == "B" & itemData$frage == item,], serie + frage ~ subitem, value.var = "wrong")[, 3:7])
    noCountsB <- as.matrix(dcast(itemData[itemData$serie == "B" & itemData$frage == item,], serie + frage ~ subitem, value.var = "no_answer")[, 3:7])
  
    a1 <- matrix(c(correctCountsA[, c(5,4,3,2,1)], wrongCountsA[, c(5,4,3,2,1)], noCountsA[, c(5,4,3,2,1)]), ncol = 5, byrow = T)
    bp <- barplot(a1, horiz = T, names.arg = rev(c("A", "B", "C", "D", "E")), col = c("green", "red", "grey"), main = paste("Frage", item, "A"), xlab = "Anzahl Antworten", ylab = "Antwortmöglichkeit", cex.names = 0.8, cex.axis = 0.8, las = 1)
    all <- correctCountsA + wrongCountsA + noCountsA
    text(rev(correctCountsA/2), bp, rev(round(correctCountsA / all * 100)), cex = 0.8)
    text(rev(correctCountsA + wrongCountsA/2), bp, rev(round(wrongCountsA / all * 100)), cex = 0.8)
    text(rev(correctCountsA + wrongCountsA + noCountsA/2), bp, rev(round(noCountsA / all * 100)), cex = 0.8)
  
    b1 <- matrix(c(correctCountsB[, c(5,4,3,2,1)], wrongCountsB[, c(5,4,3,2,1)], noCountsB[, c(5,4,3,2,1)]), ncol = 5, byrow = T)
    bp <- barplot(b1, horiz = T, names.arg = rev(c("A", "B", "C", "D", "E")), col = c("green", "red", "grey"), main = paste("Frage", item, "B"), xlab = "Anzahl Antworten", ylab = "Antwortmöglichkeit", cex.names = 0.8, cex.axis = 0.8, las = 1)
    all <- correctCountsB + wrongCountsB + noCountsB
    text(rev(correctCountsB/2), bp, rev(round(correctCountsB / all * 100)), cex = 0.8)
    text(rev(correctCountsB + wrongCountsB/2), bp, rev(round(wrongCountsB / all * 100)), cex = 0.8)
    text(rev(correctCountsB + wrongCountsB + noCountsB/2), bp, rev(round(noCountsB / all * 100)), cex = 0.8)
  }
}
dev.off()

