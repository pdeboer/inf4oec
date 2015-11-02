library(shiny)
library(DT)
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
  output$histogramRawScores <- renderPlot ({
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
  })
  
  # compute correctness
  correctness <- reactive ({
    
    temp <- dataReordered()
    
    # exclude empty tests
    empty <- apply(temp, 1, function(x) ifelse(var(x) == 0, TRUE, FALSE))
    temp <- temp[!empty, ]
    
    # compare answers with solution
    data.frame(t(apply(temp, 1, function(x) ifelse(x == solution(), ifelse(itemTypes() == "Multiple Choice" & x == 0, 0, 1), ifelse(itemTypes() == "Multiple Choice" & x == 0, 0, -1)))))
    
  })
  
  # preparing data for plots
  plotData <- reactive ({
    correct <- colSums(correctness()==1)
    noAnswer <- colSums(correctness()==0)
    incorrect <- colSums(correctness()==-1)
    rbind(correct, incorrect, noAnswer)
  })
  
  # generate plots
  for (i in 1:160)
  {
    local ({
      iLocal <- i
      plotname <- paste0("plot", iLocal)
      output[[plotname]] <- renderPlot ({
        par(mar = c(0, 0, 0, 0))
        currentPlot <- barplot(as.matrix(plotData()[ , iLocal]), col = c("Green", "Red", "Gray"), horiz = TRUE, axes = FALSE, main = NULL, border = FALSE)
        text(0, currentPlot, plotData()[1 , iLocal], cex = 1, pos = 4)
        text(plotData()[1 , iLocal], currentPlot, plotData()[2 , iLocal], cex = 1, pos = 4)
        if (plotData()[3 , iLocal] > 0)
        {
          text(plotData()[1 , iLocal] + plotData()[2 , iLocal], currentPlot, plotData()[3 , iLocal], cex = 1, pos = 4)
        }
        dev.off()
      })
    })
  }
  
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
  
  # OUTPUT: table to identify ambigous items
  output$itemTable <- renderUI ({
    
    index <- seq(1, 160, 5)
    
    temp <- paste0('
      <table class="table">
        <thead>
          <tr>
            <th width="10%">
              Item
            </th>
            <th width="40%" style="padding-left: 15px">
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
          <tr style = "background: #F5F5F5; height: 80px; font-size: 150%">
            <td style="vertical-align: middle">
              ', i, '
            </td>
            <td style="vertical-align: middle; padding-left: 15px">
              Difficulty: ', itemDifficulties()[i], '
            </td>
            <td style="vertical-align: middle;">
              Item Total Correlation: ', itemTotalCorrelations()[i], '
            </td>
            <td style="vertical-align: middle; text-align: center">
              <input id="out', index[i], '" type="checkbox" checked="checked"/>
            </td>
          </tr>')
        
        for (j in 1:itemAlternatives()[index[i]])
        {
          temp <- paste0(temp, '
            <tr style = "background: white; font-weight: lighter;">
              <td style="vertical-align: middle">
                ', i, '.', j, '
              </td>
              <td style="vertical-align: middle" colspan = "2">
                <div id="plot', index[i] + j - 1, '" class="shiny-plot-output" style="height:50px"/>
              </td>
              <td style="vertical-align: middle">
              </td>
            </tr>')
        }
      }
      else
      {
        temp <- paste0(temp, '
          <tr style = "background: #F5F5F5; height: 80px; font-size: 150%">
            <td style="vertical-align: middle">
              ', i, '
            </td>
            <td style="vertical-align: middle; padding-left: 15px">
              Difficulty: ', itemDifficulties()[i], '
            </td>
            <td style="vertical-align: middle;">
              Item Total Correlation: ', itemTotalCorrelations()[i], '
            </td>
            <td style="vertical-align: middle; text-align: center">
            </td>
          </tr>')
        
        for (j in 1:itemAlternatives()[index[i]])
        {
          temp <- paste0(temp, '
            <tr style = "background: white; font-weight: lighter;">
              <td style="vertical-align: middle">
                ', i, '.', j, '
              </td>
              <td style="vertical-align: middle" colspan = "2">
                <div id="plot', index[i] + j - 1, '" class="shiny-plot-output" style="height:50px"/>
              </td>
              <td style="vertical-align: middle; text-align: center">
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
    
  })
  
  # out items
  outItems <- reactive ({
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
  })
  
  # update correctness
  correctnessFinal <- reactive ({
    temp <- correctness()
    if (!is.null(outItems()))
    {
      temp[outItems()] <- 1
    }
    temp
  })
  
  # total scores
  scores <- reactive ({
    
    pointsPerAlternative <- itemPoints() / itemAlternatives()
    scores <- data.frame(mapply('*', correctnessFinal(), pointsPerAlternative), row.names = rownames(correctnessFinal()))
    
    index <- seq(1, 160, 5)
    scoresRounded <- sapply(index, function(x) ifelse(itemTypes()[x] == "Single Choice" & rowSums(scores[ , x:(x + 4)]) != itemPoints()[x], 0, round25(rowSums(scores[ , x:(x + 4)]))))
    scoresRounded <- ifelse(scoresRounded < 0, 0, scoresRounded)
    scoresRounded <- data.frame(rowSums(scoresRounded))
  })
  
  # grades slider
  output$slider4 <- renderUI ({
    sliderInput("points4", label=NULL, min=0, max=sum(itemPoints()) / 5, value=c(sum(itemPoints()) / 5 / 2, sum(itemPoints()) / 5), step=0.25)
  })
  
  
  # grading slope
  slope <- reactive ({
    if (!is.null(input$points4) && !is.null(input$points6))
    {
      calculateSlope(input$points4, 3.875, input$points6, 5.875)
    }
  })
  
  # grading intercept
  intercept <- reactive ({
    if (!is.null(slope()) && !is.null(input$points4))
    {
      calculateIntercept(3.875, slope(), input$points4)
    }
  })
  
  # compute grades
  grades <- reactive ({
    temp <- scores()
    temp$note <- round25(calculateY(slope(), temp[ , 1], intercept()))
    temp$note <- ifelse(temp$note > 6, 6, temp$note)
    temp$note <- ifelse(temp$note < 1, 1, temp$note)
    temp
  })
  
  # mode
  mode <- reactive ({
    if (!is.null(grades()))
    {
      max(sort(table(grades()[ , 2]), decreasing=TRUE))
    }
  })
  
  # plot grades
  output$grades <- renderPlot ({
    if (!is.null(grades()) && !is.null(mode()))
    {
      h <- hist(grades()[ , 2], breaks=seq(0.875,6.125,0.25))
      c = ifelse(h$mids < 4, "red", "green");
      plot(h, main=NULL, xlab="Note", ylab="Häufigkeit", xlim=c(0.75,6.25), ylim=c(0,mode()+10), col=c, border="white", labels=TRUE)
    }
  })
  
  output$blii <- renderDataTable({
    grades()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # OUTPUT: download handler for the report
  output$report = downloadHandler (
    filename = "Report.pdf",
    content = function(file)
    {
      out = knit2pdf('template.rnw', clean = TRUE, encoding="UTF-8")
      file.rename(out, file) # move pdf to file for downloading
      file.remove("template.tex")
      file.remove("figure/figure1-1.pdf", "figure/figure2-1.pdf", "figure/figure3-1.pdf", "figure/figure4-1.pdf")
      file.remove("figure")
    },
    contentType = 'application/pdf'
  )
  
})