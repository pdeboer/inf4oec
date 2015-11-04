library(shiny)
library(reshape2)
library(knitr)
library(xtable)
library(seqinr) #used in knitr template

# returns the slope of a linear function
calculateSlope <- function (x1, y1, x2, y2)
{
  return ((y2 - y1) / (x2 - x1))
}

# returns the intercept of a linear function
calculateIntercept <- function (y, m, x)
{
  return (y - m * x)
}

# returns y based on m, x, q
calculateY <- function (m, x, q)
{
  return (m * x + q)
}

# returns a value rounded to fourths
round25 <- function (n)
{
  return (round(n / 0.25) * 0.25)
}

# returns a vector of item difficulties, rounded to 2 digits
itemDiff <- function (itemData, maxPoints)
{
  return (round(colSums(itemData) / (nrow(itemData) * maxPoints), 2))
}

# returns a vector of item total correlations, rounded to 2 digits
itemTotalCorr <- function (itemData)
{
  itemDataCorr <- apply(itemData, 2, function(x)
  {
    rowSums(itemData) - x
  })
  return (round(diag(cov(itemData, itemDataCorr) / (apply(itemData, 2, sd) * apply(itemDataCorr, 2, sd))), 2))
}

shinyServer(function(input, output, session) {
  
  # read csv
  rawData <- reactive ({
    if (!is.null(input$inputFile))
    {
      read.csv(input$inputFile$datapath)
    }
  })
  
  # number of items
  numberOfItems <- reactive ({
    if (!is.null(rawData()))
    {
      max(rawData()$Fragennummer)
    }
  })
  
  # total number of students
  numberOfStudents <- reactive ({
    if (!is.null(rawData()) && !is.null(numberOfItems()))
    {
      nrow(rawData()) %/% numberOfItems()
    }
  })
  
  # matrikelnummern of empty tests
  emptyTests <- reactive ({
    if (!is.null(rawData()))
    {
      temp <- dcast(rawData(), rawData()$liebmannnr ~ rawData()$Fragennummer, value.var = "Punkte")
      temp[rowSums(temp[-1]) == 0, 1]
    }
  })
  
  # data without empty tests
  data <- reactive ({
    if (!is.null(rawData()) && !is.null(emptyTests()))
    {
      rawData()[!rawData()$liebmannnr %in% emptyTests(), ]
    }
  })
  
  # OUTPUT: upload confirmation
  output$confirmation <- renderUI ({
    if (!is.null(numberOfItems()) && !is.null(numberOfStudents()) && !is.null(emptyTests()))
    {
      if (numberOfItems() > 0 && numberOfStudents() > 0)
      {
        HTML(paste0('<font color = "green"><b>', numberOfStudents(), '</b> students and <b>', numberOfItems(), '</b> items found. <b>', length(emptyTests()), '</b> tests are empty and will be excluded for the analysis.</font>'))
      }
      else
      {
        HTML('<font color = "red">Error: Please check file structure.</font>')
      }
    }
  })
  
  # data of series A
  dataA <- reactive ({
    if (!is.null(data()))
    {
      split(data(), data()$Serie)$A
    }
  })
  
  # data of series B
  dataB <- reactive ({
    if (!is.null(data()))
    {
      split(data(), data()$Serie)$B
    }
  })
  
  # item types
  itemTypes <- reactive ({
    if (!is.null(data()))
    {
      rep(ifelse(rowSums(aggregate(. ~ Fragennummer, data(), function(x) -1 %in% x)[4:8]) > 0, "Multiple Choice", "Single Choice"), each = 5)
    }
  })
  
  # guess max. points for each item
  itemPointsGuess <- reactive ({
    if (!is.null(data()))
    {
      rep(aggregate(Punkte ~ Fragennummer, data(), max)$Punkte, each = 5)
    }
  })
  
  # guess number of alternatives for each item
  itemAlternatives <- reactive ({
    if (!is.null(data()))
    {
      rep(rowSums(aggregate(. ~ Fragennummer, data(), function(x) -1 %in% x || 1 %in% x)[4:8]), each = 5)
    }
  })
  
  # guess solution of series A
  solutionGuess <- reactive ({
    if (!is.null(dataA()))
    {
      solution <- unique(merge(dataA(), aggregate(Punkte ~ Fragennummer, dataA(), max))[c(1, 5:9)])
      as.vector(t(solution[order(solution$Fragennummer), -1]))
    }
  })
  
  # OUTPUT: table to validate guessed information
  output$validationTable <- renderUI ({
    
    if (!is.null(numberOfItems()) && !is.null(itemPointsGuess()) && !is.null(itemTypes()) && !is.null(itemAlternatives()) && !is.null(solutionGuess()))
    {
    
    index <- seq(1, numberOfItems() * 5, 5)
    
    temp <- paste0('
      <table class="table">
        <thead>
          <tr>
            <th width="20%">
              Item
            </th>
            <th width="40%">
              Type / Solution (Series A)
            </th>
            <th width="40%">
              Points (', sum(itemPointsGuess()[index]), ') / Corresponding (Series B)
            </th>
          </tr>
        </thead>
        <tbody>')
    
    for (i in 1:numberOfItems())
    {
      temp <- paste0(temp, '
        <tr style = "background: #F5F5F5">
          <td style="vertical-align: middle">
            ', i, '
          </td>
          <td style="vertical-align: middle">
            ', itemTypes()[index[i]], '
          </td>
          <td style="vertical-align: middle">
            <div class="form-group shiny-input-container">
              <input id="itemPoints', i,'" type="number" class="form-control" value="', itemPointsGuess()[index[i]], '"/>
            </div>
          </td>
        </tr>')
      
      for (j in 1:itemAlternatives()[index[i]])
      {
        if (itemTypes()[index[i]] == "Single Choice")
        {
          temp <- paste0(temp, '
            <tr style = "background: white; font-weight: lighter;">
              <td style="vertical-align: middle">
                ', i, '.', j, '
              </td>')
          if (solutionGuess()[index[i] + j - 1] == 0)
          {
            temp <- paste0(temp, '
              <td style="vertical-align: middle">

                <div id="solution', index[i] + j - 1, '" class="form-group shiny-input-radiogroup shiny-input-container">
                  <div class="shiny-options-group">
                    <div class="radio">
                      <label>
                        <input type="radio" name="solution', index[i] + j - 1, '" value="0" checked="checked"/>
                        <span>Incorrect</span>
                      </label>
                    </div>
                    <div class="radio">
                      <label>
                        <input type="radio" name="solution', index[i] + j - 1, '" value="1"/>
                        <span>Correct</span>
                      </label>
                    </div>
                  </div>
                </div>

              </td>

              <td style="vertical-align: middle">
                <div class="form-group shiny-input-container">
                  <input id="corresponding', index[i] + j - 1, '" type="number" class="form-control" value="', j, '" min="1" max="', itemAlternatives()[index[i]], '"/>
                </div>
              </td>

            </tr>')
          }
          else
          {
            temp <- paste0(temp, '
              <td style="vertical-align: middle">

                <div id="solution', index[i] + j - 1, '" class="form-group shiny-input-radiogroup shiny-input-container">
                  <div class="shiny-options-group">
                    <div class="radio">
                      <label>
                        <input type="radio" name="solution', index[i] + j - 1, '" value="0"/>
                        <span>Incorrect</span>
                      </label>
                    </div>
                    <div class="radio">
                      <label>
                        <input type="radio" name="solution', index[i] + j - 1, '" value="1" checked="checked"/>
                        <span>Correct</span>
                      </label>
                    </div>
                  </div>
                </div>

              </td>

              </td>
              <td style="vertical-align: middle">
                <div class="form-group shiny-input-container">
                  <input id="corresponding', index[i] + j - 1, '" type="number" class="form-control" value="', j, '" min="1" max="', itemAlternatives()[index[i]], '"/>
                </div>
              </td>
            </tr>')
          }
        }
        else
        {
          temp <- paste0(temp, '
            <tr style = "background: white; font-weight: lighter;">
              <td style="vertical-align: middle">
                ', i, '.', j, '
              </td>')
          if (solutionGuess()[index[i] + j - 1] == -1)
          {
            temp <- paste0(temp, '
              <td style="vertical-align: middle">

                <div id="solution', index[i] + j - 1, '" class="form-group shiny-input-radiogroup shiny-input-container">
                  <div class="shiny-options-group">
                    <div class="radio">
                      <label>
                        <input type="radio" name="solution', index[i] + j - 1, '" value="-1" checked="checked"/>
                        <span>Incorrect</span>
                      </label>
                    </div>
                    <div class="radio">
                      <label>
                        <input type="radio" name="solution', index[i] + j - 1, '" value="1"/>
                        <span>Correct</span>
                      </label>
                    </div>
                  </div>
                </div>

              </td>

              </td>
              <td style="vertical-align: middle">
                <div class="form-group shiny-input-container">
                  <input id="corresponding', index[i] + j - 1, '" type="number" class="form-control" value="', j, '" min="1" max="', itemAlternatives()[index[i]], '"/>
                </div>
              </td>
            </tr>')
          }
          else
          {
            temp <- paste0(temp, '
              <td style="vertical-align: middle">

                <div id="solution', index[i] + j - 1, '" class="form-group shiny-input-radiogroup shiny-input-container">
                  <div class="shiny-options-group">
                    <div class="radio">
                      <label>
                        <input type="radio" name="solution', index[i] + j - 1, '" value="-1"/>
                        <span>Incorrect</span>
                      </label>
                    </div>
                    <div class="radio">
                      <label>
                        <input type="radio" name="solution', index[i] + j - 1, '" value="1" checked="checked"/>
                        <span>Correct</span>
                      </label>
                    </div>
                  </div>
                </div>

              </td>

              </td>
              <td style="vertical-align: middle">
                <div class="form-group shiny-input-container">
                  <input id="corresponding', index[i] + j - 1, '" type="number" class="form-control" value="', j, '" min="1" max="', itemAlternatives()[index[i]], '"/>
                </div>
              </td>
            </tr>')
          }
        }
      }
    }
    
    temp <- paste0(temp, '
        </tbody>
      </table>
    ')
    
    HTML(temp)
    
    }
    
  })
  
  # max. points for each item
  itemPoints <- reactive ({
    if (!is.null(numberOfItems()) && !is.null(input$itemPoints1))
    {
      temp <- c()
      for (i in 1:numberOfItems())
      {
        temp <- c(temp, eval(parse(text = paste0('input$itemPoints', i))))
      }
      rep(temp, each = 5)
    }
  })
  
  # solution of series A
  solution <- reactive ({
    if (!is.null(numberOfItems()) && !is.null(input$solution1))
    {
      temp <- c()
      for (i in 1:(numberOfItems() * 5))
      {
        if (!is.null(eval(parse(text = paste0('input$solution', i)))))
        {
          temp <- c(temp, as.integer(eval(parse(text = paste0('input$solution', i)))))
        }
        else
        {
          temp <- c(temp, as.integer(0))
        }
      }
      temp
    }
  })
  
  # corresponding alternatives of series B
  corresponding <- reactive ({
    if (!is.null(numberOfItems()) && !is.null(input$corresponding1))
    {
      temp <- c()
      index <- seq(1, numberOfItems() * 5, 5)
      for (i in 1:numberOfItems())
      {
        for (j in 1:5)
        {
          if (!is.null(eval(parse(text = paste0('input$corresponding', index[i] + j - 1)))))
          {
            temp <- c(temp, as.integer(index[i] + eval(parse(text = paste0('input$corresponding', index[i] + j - 1))) - 1))
          }
          else
          {
            temp <- c(temp, as.integer(index[i] + j - 1))
          }
        }
      }
      temp
    }
  })
  
  # item data
  itemData <- reactive ({
    if (!is.null(data()))
    {
      dcast(data(), data()$liebmannnr ~ data()$Fragennummer, value.var = "Punkte")[-1]
    }
  })
  
  # OUTPUT: histogram of raw scores
  output$histRawScores <- renderPlot ({
    if (!is.null(itemData()) && !is.null(itemPoints()))
    {
      totalPoints <- sum(itemPoints()) / 5
      h <- hist(rowSums(itemData()), main="Histogram", xlab="Points", ylab="Frequency", col = rgb(8/256, 119/256, 189/256), breaks = totalPoints/5, xlim=c(0, totalPoints), xaxt='n')
      axis(1, at=seq(0, totalPoints, 5))
      xfit <- seq(0, totalPoints, length=totalPoints)
      yfit <- dnorm(xfit, mean=mean(rowSums(itemData())), sd=sd(rowSums(itemData())))
      yfit <- yfit*diff(h$mids[1:2])*length(rowSums(itemData()))
      lines(xfit, yfit)
      abline(v = mean(rowSums(itemData())), col = "orange", lwd = 1)
      abline(v = median(rowSums(itemData())), col = "red", lwd = 1)
      legend("topright", legend = c("Mean", "Median"), col = c("orange", "red"), lwd=1)
    }
  })
  
  # OUTPUT: q-q plot of raw scores
  output$qqRawScores <- renderPlot ({
    if (!is.null(itemData()))
    {
      qqnorm(rowSums(itemData()), main="Normal Q-Q plot", xlab="Expected Quantiles", ylab="Observed Quantiles")
      qqline(rowSums(itemData()), col=2)
    }
  })
  
  # reorder data (based on corresponding alternatives of series B)
  dataReordered <- reactive ({
    if (!is.null(data()) && !is.null(corresponding()))
    {
      # reshape A
      tempA <- dataA()
      tempA$Pattern <- paste(tempA$A, tempA$B, tempA$C, tempA$D, tempA$E)
      tempA <- dcast(tempA, liebmannnr ~ Fragennummer, value.var = "Pattern")
      tempA <- data.frame(apply(tempA[-1], 2, function(x) colsplit(x, " ", 1:5)), row.names = tempA[ , 1])
      
      # reshape B
      tempB <- dataB()
      tempB$Pattern <- paste(tempB$A, tempB$B, tempB$C, tempB$D, tempB$E)
      tempB <- dcast(tempB, liebmannnr ~ Fragennummer, value.var = "Pattern")
      tempB <- data.frame(apply(tempB[-1], 2, function(x) colsplit(x, " ", 1:5)), row.names = tempB[ , 1])
      
      # sort B
      tempB <- tempB[ , corresponding()]
      
      # rename and bind A/B
      colnames(tempA) <- 1:160
      colnames(tempB) <- 1:160
      rbind(tempA, tempB)
    }
  })
  
  # compute correctness
  correctness <- reactive ({
    if (!is.null(dataReordered()) && !is.null(solution()) && !is.null(itemTypes()))
    {
      temp <- dataReordered()
      data.frame(t(apply(temp, 1, function(x) ifelse(x == solution(), ifelse(itemTypes() == "Multiple Choice" & x == 0, 0, 1), ifelse(itemTypes() == "Multiple Choice" & x == 0, 0, -1)))))
    }
  })
  
  # prepare data for plots
  plotData <- reactive ({
    if (!is.null(correctness()))
    {
      correct <- colSums(correctness()==1)
      noAnswer <- colSums(correctness()==0)
      incorrect <- colSums(correctness()==-1)
      rbind(correct, incorrect, noAnswer)
    }
  })
  
  # generate plots
  observe ({
    if(!is.null(numberOfItems()))
    {
      for (i in 1:(numberOfItems() * 5))
      {
        local ({
          iLocal <- i
          plotName <- paste0("plot", iLocal)
          output[[plotName]] <- renderPlot ({
            par(mar = c(0, 0, 0, 0))
            currentPlot <- barplot(as.matrix(plotData()[ , iLocal]), col = c("Green", "Red", "Gray"), horiz = TRUE, axes = FALSE, main = NULL, border = FALSE)
            if (plotData()[1 , iLocal] > 25)
            {
              text(0, currentPlot, plotData()[1 , iLocal], cex = 1, pos = 4)
            }
            if (plotData()[2 , iLocal] > 25)
            {
              text(plotData()[1 , iLocal], currentPlot, plotData()[2 , iLocal], cex = 1, pos = 4)
            }
            if (plotData()[3 , iLocal] > 25)
            {
              text(plotData()[1 , iLocal] + plotData()[2 , iLocal], currentPlot, plotData()[3 , iLocal], cex = 1, pos = 4)
            }
            dev.off()
          })
        })
      }
    }
  })
  
  # item difficulties
  itemDifficulties <- reactive ({
    if (!is.null(itemData()) & !is.null(itemPoints()))
    {
      itemDiff(itemData(), itemPoints()[c(TRUE, rep(FALSE, 4))])
    }
  })
  
  # item total correlations
  itemTotalCorrelations <- reactive ({
    if (!is.null(itemData()))
    {
      itemTotalCorr(itemData())
    }
  })
  
  # OUTPUT: table with item statistics
  output$itemTable <- renderUI ({
    
    if (!is.null(numberOfItems()) && !is.null(itemTypes()) && !is.null(itemDifficulties()) && !is.null(itemTotalCorrelations()) && !is.null(itemAlternatives()))
    {
    
    index <- seq(1, numberOfItems() * 5, 5)
    
    temp <- paste0('
      <table class="table">
        <thead>
          <tr>
            <th width="10%">
              Item
            </th>
            <th width="40%" style = "padding-left: 15px">
              Difficulty
            </th>
            <th width="40%">
              Item Total Correlation
            </th>
            <th width="10%">
            </th>
          </tr>
        </thead>
        <tbody>')
    
    for (i in 1:numberOfItems())
    {
      if (itemTypes()[index[i]] == "Single Choice")
      {
        temp <- paste0(temp, '
          <tr style = "background: #F5F5F5; height: 50px;">
            <td style = "vertical-align: middle">
              ', i, '
            </td>
            <td style = "vertical-align: middle; padding-left: 15px;">
              ', itemDifficulties()[i], '
            </td>
            <td style = "vertical-align: middle">
              ', itemTotalCorrelations()[i], '
            </td>
            <td style = "vertical-align: middle; text-align: center;">
              <input id="out', index[i], '" type="checkbox" checked="checked"/>
            </td>
          </tr>')
        
        for (j in 1:itemAlternatives()[index[i]])
        {
          temp <- paste0(temp, '
            <tr style = "background: white; font-weight: lighter;">
              <td style = "vertical-align: middle">
                ', i, '.', j, '
              </td>
              <td style = "vertical-align: middle" colspan = "2">
                <div id="plot', index[i] + j - 1, '" class="shiny-plot-output" style = "height: 25px"/>
              </td>
              <td style = "vertical-align: middle">
              </td>
            </tr>')
        }
      }
      else
      {
        temp <- paste0(temp, '
          <tr style = "background: #F5F5F5; height: 50px;">
            <td style = "vertical-align: middle">
              ', i, '
            </td>
            <td style = "vertical-align: middle; padding-left: 15px;">
              ', itemDifficulties()[i], '
            </td>
            <td style = "vertical-align: middle">
              ', itemTotalCorrelations()[i], '
            </td>
            <td style = "vertical-align: middle; text-align: center;">
            </td>
          </tr>')
        
        for (j in 1:itemAlternatives()[index[i]])
        {
          temp <- paste0(temp, '
            <tr style = "background: white; font-weight: lighter;">
              <td style = "vertical-align: middle">
                ', i, '.', j, '
              </td>
              <td style = "vertical-align: middle" colspan = "2">
                <div id="plot', index[i] + j - 1, '" class="shiny-plot-output" style = "height: 25px"/>
              </td>
              <td style = "vertical-align: middle; text-align: center">
                <input id="out', index[i] + j - 1, '" type="checkbox" checked="checked"/>
              </td>
            </tr>')
        }
      }
    }
    
    temp <- paste0(temp, '
        </tbody>
      </table>
    ')
    
    HTML(temp)
    
    }
    
  })
  
  # out items
  outItems <- reactive ({
    if (!is.null(numberOfItems()) && !is.null(input$out1) && !is.null(itemTypes()) && !is.null(itemAlternatives()))
    {
      temp <- c()
      for (i in 1:(numberOfItems() * 5))
      {
        if (!is.null(eval(parse(text = paste0('input$out', i)))) && !eval(parse(text = paste0('input$out', i))))
        {
          if (itemTypes()[i] == "Single Choice")
          {
            temp <- c(temp, as.integer(c(i:(i + itemAlternatives()[i] - 1))))
          }
          else
          {
            temp <- c(temp, as.integer(i))
          }
        }
      }
      temp
    }
  })
  
  # out items vector for report
  outItemsText <- reactive ({
    temp <- c()
    i <- 1
    while (i < (length(outItems()) + 1))
    {
      if (itemTypes()[outItems()[i]] == "Single Choice")
      {
        temp <- c(temp, as.integer(outItems()[i] / 5 + 1))
        i <- i + 5
      }
      else
      {
        if (outItems()[i] %% 5 == 0)
        {
          temp <- c(temp, outItems()[i] / 5 + 0.5)
        }
        else
        {
          digits <- strsplit(as.character(outItems()[i] / 5), "")
          last <- as.integer(digits[[1]][length(digits[[1]])]) %/% 2
          temp <- c(temp, as.integer(outItems()[i] / 5 + 1) + 0.1 * last)
        }
        i <- i + 1
      }
    }
    print(class(temp))
    temp
  })
  
  # update correctness
  correctnessFinal <- reactive ({
    if (!is.null(correctness()))
    {
      temp <- correctness()
      if (!is.null(outItems()))
      {
        temp[outItems()] <- 1
      }
      temp
    }
  })
  
  # total scores
  scores <- reactive ({
    if (!is.null(correctnessFinal()) && !is.null(itemPoints()) && !is.null(itemAlternatives()))
    {
      pointsPerAlternative <- itemPoints() / itemAlternatives()
      scoresPerAlternative <- data.frame(mapply('*', correctnessFinal(), pointsPerAlternative), row.names = rownames(correctnessFinal()))
      
      index <- seq(1, 160, 5)
      scoresPerItem <- sapply(index, function(x) ifelse(itemTypes()[x] == "Single Choice" & rowSums(scoresPerAlternative[ , x:(x + 4)]) != itemPoints()[x], 0, round25(rowSums(scoresPerAlternative[ , x:(x + 4)]))))
      scoresPerItem <- ifelse(scoresPerItem < 0, 0, scoresPerItem)
      
      data.frame(rowSums(scoresPerItem))
    }
  })
  
  # grades slider
  output$slider46 <- renderUI ({
    if (!is.null(itemPoints()))
    {
      sliderInput("points46", label=NULL, min=0, max=sum(itemPoints()) / 5, value=c(sum(itemPoints()) / 5 / 3, sum(itemPoints()) / 5), step=0.25)
    }
  })
  
  # grading slope
  slope <- reactive ({
    if (!is.null(input$points46))
    {
      calculateSlope(input$points46[1], 3.875, input$points46[2], 5.875)
    }
  })
  
  # grading intercept
  intercept <- reactive ({
    if (!is.null(slope()) && !is.infinite(slope()) && !is.null(input$points46))
    {
      calculateIntercept(3.875, slope(), input$points46[1])
    }
  })
  
  # compute grades
  grades <- reactive ({
    if (!is.null(scores()) && !is.null(slope()) && !is.null(intercept()) && !is.null(emptyTests()))
    {
      temp1 <- scores()
      temp1$Grade <- round25(calculateY(slope(), temp1[ , 1], intercept()))
      temp1$Grade <- ifelse(temp1$Grade > 6, 6, temp1$Grade)
      temp1$Grade <- ifelse(temp1$Grade < 1, 1, temp1$Grade)
      colnames(temp1) <- c("Points", "Grade")
      
      # add empty tests again
      temp2 <- emptyTests()
      temp2 <- data.frame(rep(0, length(emptyTests())), 1, row.names = emptyTests())
      colnames(temp2) <- c("Points", "Grade")
      
      rbind(temp1, temp2)
    }
  })
  
  # grades mean
  gradesMean <- reactive ({
    if (!is.null(grades()))
    {
      round(mean(grades()[ , 2]), 2)
    }
  })
  
  # number passed
  numberPassed <- reactive ({
    if (!is.null(grades()))
    {
      sum(grades()[ , 2] > 3.75)
    }
  })
  
  # number failed
  numberFailed <- reactive ({
    if (!is.null(grades()))
    {
      sum(grades()[ , 2] < 4)
    }
  })
  
  # OUTPUT: plot grades
  output$histGrades <- renderPlot ({
    if (!is.null(grades()) && !is.null(emptyTests()))
    {
      percentPassed <- round(100 * numberPassed() / (numberOfStudents() - length(emptyTests())), 2)
      percentFailed <- 100 - percentPassed
      mode <- max(sort(table(grades()[ , 2]), decreasing=TRUE))
      
      h <- hist(grades()[ , 2], breaks=seq(0.875,6.125,0.25))
      c = ifelse(h$mids < 4, "red", "green");
      plot(h, main=NULL, xlab="Grade", ylab="Frequency", xlim=c(0.75,6.25), ylim=c(0,mode+10), col=c, border="white", labels=TRUE)
      
      segments(x0 = mean(grades()[ , 2]), y0 = 1, y1 = 25, col = "black", lwd = 3, lty = 3)
      legend("topright", legend = c(paste0("Pass: ", numberPassed(), " (", percentPassed, "%)"), paste0("Fail: ", numberFailed() - length(emptyTests()), " (", percentFailed, "%)"), paste0("Mean: ", round(mean(grades()[ , 2]), 2))), col = c("green", "red", "black"), lty = c(1, 1, 3))
    }
  })
  
  # OUTPUT: download handlers
  observe ({
    if (!is.null(grades()))
    {
      
      # OUTPUT: download handler for grades (.csv)
      output$csv <- downloadHandler (
        filename = "Grades.csv",
        content = function(file)
        {
          write.csv(grades(), file, row.names = TRUE, fileEncoding="UTF-8")
        }
      )
      
      # OUTPUT: download handler for report (.pdf)
      output$report = downloadHandler (
        filename = "Report.pdf",
        content = function(file)
        {
          out = knit2pdf('template.rnw', clean = TRUE, encoding="UTF-8")
          file.rename(out, file) # move pdf to file for downloading
          file.remove("template.tex")
          #file.remove("figure/figure1-1.pdf", "figure/figure2-1.pdf", "figure/figure3-1.pdf", "figure/figure4-1.pdf")
          #file.remove("figure")
        },
        contentType = 'application/pdf'
      )
      
    }
    
  })
  
})
