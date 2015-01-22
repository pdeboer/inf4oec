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
itemDifficulties <- function (itemData, maxPoints)
{
  return (round(colSums(itemData) / (nrow(itemData) * maxPoints), 2))
}

# returns a vector of item total correlations, rounded to 2 digits
itemTotalCorrelations <- function (itemData)
{
  itemDataCorr <- apply(itemData, 2, function(x)
  {
    rowSums(itemData) - x
  })
  return (round(diag(cov(itemData, itemDataCorr) / (apply(itemData, 2, sd) * apply(itemDataCorr, 2, sd))), 2))
}

shinyServer(function(input, output, session) {
  
  # observe all the navigation buttons and change the tab on click
  observe ({ if (input$next1 != 0) updateTabsetPanel(session, "main", selected = "Schritt 1: Daten einlesen") })
  observe ({ if (input$back0 != 0) updateTabsetPanel(session, "main", selected = "Information") })
  observe ({ if (input$next2 != 0) updateTabsetPanel(session, "main", selected = "Schritt 2: Daten transponieren") })
  observe ({ if (input$back1 != 0) updateTabsetPanel(session, "main", selected = "Schritt 1: Daten einlesen") })
  observe ({ if (input$next3 != 0) updateTabsetPanel(session, "main", selected = "Schritt 3: Maximal erreichbare Punkte") })
  observe ({ if (input$back2 != 0) updateTabsetPanel(session, "main", selected = "Schritt 2: Daten transponieren") })
  observe ({ if (input$next4 != 0) updateTabsetPanel(session, "main", selected = "Schritt 4: Leere Prüfungen") })
  observe ({ if (input$back3 != 0) updateTabsetPanel(session, "main", selected = "Schritt 3: Maximal erreichbare Punkte") })
  observe ({ if (input$next5 != 0) updateTabsetPanel(session, "main", selected = "Schritt 5: Rohwertverteilung") })
  observe ({ if (input$back4 != 0) updateTabsetPanel(session, "main", selected = "Schritt 4: Leere Prüfungen") })
  observe ({ if (input$next6 != 0) updateTabsetPanel(session, "main", selected = "Schritt 6: Itemkennwerte") })
  observe ({ if (input$back5 != 0) updateTabsetPanel(session, "main", selected = "Schritt 5: Rohwertverteilung") })
  observe ({ if (input$next7 != 0) updateTabsetPanel(session, "main", selected = "Schritt 7: Notengebung") })
  observe ({ if (input$back6 != 0) updateTabsetPanel(session, "main", selected = "Schritt 6: Itemkennwerte") })
  observe ({ if (input$next8 != 0) updateTabsetPanel(session, "main", selected = "Schritt 8: Bericht") })
  observe ({ if (input$back7 != 0) updateTabsetPanel(session, "main", selected = "Schritt 7: Notengebung") })
  observe ({ if (input$next0 != 0) updateTabsetPanel(session, "main", selected = "Information") })
  
  # read the csv provided by the user to a data frame
  rawData <- reactive ({
    if (!is.null(input$inputFile))
    {
      read.csv(input$inputFile$datapath, header=input$header, sep=input$sep, quote=input$quote, encoding="UTF-8")
    }
  })
  
  # OUTPUT: table of rawData
  output$table_rawData <- renderDataTable ({
    if (!is.null(rawData()))
    {
      rawData()
    }
  })
  
  # transpose rawData to a data frame with one column for the Matrikelnummer and a column for every item
  allData <- reactive ({
    if (!is.null(rawData()))
    {
      temp <- dcast(rawData(), eval(parse(text = colnames(rawData())[1])) ~ eval(parse(text = colnames(rawData())[3])), value.var = colnames(rawData())[ncol(rawData())])
      if (ncol(temp) > 2)
      {
        temp
      }
    }
  })
  
  # number of students to take the exam
  numberOfStudents <- reactive ({
    if (!is.null(allData()))
    {
      nrow(allData())
    }
  })
  
  # number of invalid exams (points achieved == 0)
  numberOfInvalidExams <- reactive ({
    if (!is.null(allData()))
    {
      sum(rowSums(allData()[ , 2:ncol(allData())]) == 0)
    }
  })
  
  # number of valid exams (point achieved > 0)
  numberOfValidExams <- reactive ({
    if (!is.null(numberOfStudents()) && !is.null(numberOfInvalidExams()))
    {
      numberOfStudents() - numberOfInvalidExams()
    }
  })
  
  # number of items
  numberOfItems <- reactive ({
    if (!is.null(allData()))
    {
      ncol(allData()) - 1
    }
  })
  
  # OUTPUT: html element with the number of students found
  output$text_numberOfStudents <- renderUI ({
    if (!is.null(numberOfStudents()))
    {
      HTML(paste0(p("Anzahl Studenten", style = "font-weight: bold"), numberOfStudents()))
    }
  })
  
  # OUTPUT: html element with the number of items found
  output$text_numberOfItems <- renderUI ({
    if (!is.null(numberOfItems()))
    {
      HTML(paste0(p("Anzahl Items", style = "font-weight: bold"), numberOfItems()))
    }
  })
  
  # allData without the column for the Matrikelnummer (gives us just the item data, depending on the decision if empty tests should be excluded or not)
  itemData <- reactive ({
    if (!is.null(allData()))
    {
      if (input$emptyTestsDecision == "Ja")
      {
        allData()[rowSums(allData()[ , 2:ncol(allData())])!=0, ][ , 2:ncol(allData())]
      }
      else
      {
        allData()[ , 2:ncol(allData())]
      }
    }
  })
  
  # vector with a guess (based on the input data) about the maximum points for each item
  maxPointsGuessVec <- reactive ({
    if (!is.null(itemData()))
    {
      apply(itemData(), 2, max)
    }
  })
  
  # OUTPUT: list of numeric inputs, default values are the guessed points for each question
  output$numericInputs <- renderUI ({
    if (!is.null(numberOfItems()) && !is.null(maxPointsGuessVec()))
    {        
      numericInputs <- list()
      for (i in 1:numberOfItems())
      {
        numericInputs[[i]] <- numericInput(inputId = paste0("max", i), label = paste("Frage", i), value = maxPointsGuessVec()[[i]], min=-1, max=90, step=1)
      }
      numericInputs
    }
  })
  
  # vector of the user validated maximum points for each question
  maxPointsValidatedVec <- reactive ({
    if (!is.null(numberOfItems()) && !is.null(input$max1))
    {
      checkNA <- TRUE
      
      for (i in 1:numberOfItems())
      {
        if (is.na(eval(parse(text = paste0('input$max', i)))))
        {
          checkNA <- FALSE
          break
        }
      }
      
      if (checkNA)
      {
        temp <- c()
        for (i in 1:numberOfItems())
        {
          temp <- append(temp, eval(parse(text = paste0('input$max', i))))
        }
        temp
      }
    }
  })
  
  # sum of the user validated maximum points for each question
  maxPointsValidatedSum <- reactive ({
    if (!is.null(maxPointsValidatedVec()))
    {
      sum(maxPointsValidatedVec())
    }
  })
  
  # OUTPUT: html element with the number of invalid exams
  output$text_numberOfEmptyTests <- renderUI ({
    if (!is.null(numberOfInvalidExams()))
    {
      HTML(paste0(p("Anzahl leere Prüfungen ", style = "font-weight: bold"), numberOfInvalidExams()))
    }
  })
  
  # OUTPUT: plot of the scores' distribution (depending on the deciision if a histogram or a qq plot should be shown)
  output$plot_distribution <- renderPlot ({
    if (!is.null(itemData()) && !is.null(maxPointsValidatedSum()))
    {
      if (input$distributionPlotDecision == "Histogramm")
      {
        h <- hist(rowSums(itemData()), main="Verteilung der Punkte", xlab="Anzahl Punkte", ylab="Häufigkeit", col = "lightblue", breaks = maxPointsValidatedSum()/5, xlim=c(0, maxPointsValidatedSum()), xaxt='n')
        axis(1, at=seq(0, maxPointsValidatedSum(), 5))
        xfit <- seq(0, maxPointsValidatedSum(), length=maxPointsValidatedSum())
        yfit <- dnorm(xfit, mean=mean(rowSums(itemData())), sd=sd(rowSums(itemData())))
        yfit <- yfit*diff(h$mids[1:2])*length(rowSums(itemData()))
        lines(xfit, yfit)
        abline(v = mean(rowSums(itemData())), col = "blue", lwd = 1)
        abline(v = median(rowSums(itemData())), col = "orange", lwd = 1)
        legend("topright", legend = c("Mittelwert", "Median"), col = c("blue", "orange"), lwd=1)
      }
      else
      {
        qqnorm(rowSums(itemData()), main="Normaler Q-Q-Plot", xlab="Erwartete Quantile", ylab="Beobachtete Quantile")
        qqline(rowSums(itemData()), col=2)
      }
    }
  })
  
  # two item statistics: item difficulty and item total correlation
  itemStats <- reactive ({
    if (!is.null(itemData()) && !is.null(maxPointsValidatedVec()))
    {
      cbind(itemDifficulties(itemData(), maxPointsValidatedVec()), itemTotalCorrelations(itemData()))
    }
  })
  
  # the default (starting) cut off value for the item total correlation (chosen such that the two items are below)
  itemCutoffDefault <- reactive ({
    if (!is.null(itemStats()))
    {
      temp <- itemStats()[order(itemStats()[ , 2]), ][2, 2] + 0.01
      if (temp > 1)
      {
        temp <- 1
      }
      else if (temp < -1)
      {
        temp <- -1
      }
      temp
    }
  })
  
  # OUTPUT: numeric input to adjust the cutoff value for the item total correlation
  output$itemCutoffInput <- renderUI ({
    if (!is.null(itemCutoffDefault()))
    {
      numericInput("itemCutoffInput", "Legen Sie den Mindestwert für die Trennschärfe fest.", min=-1, max=1, step=0.01, value=itemCutoffDefault())
    }
    else
    {
      numericInput("itemCutoffInput", "Legen Sie den Mindestwert für die Trennschärfe fest.", min=-1, max=1, step=0.01, value=0)
    }
  })
  
  output$itemStatsLayout <- renderUI ({
    if (!is.null(numberOfItems()) && !is.null(itemStats()))
    {
      layout <- list()
      layout[[1]] <- ('<table class="table table-striped table-hover dataTable", id="itemStatsTable"><thead><tr><th>Item</th><th>Schwierigkeit</th><th>Trennschärfe</th></tr></thead><tbody>')
      
      for (i in 1:numberOfItems())
      {
        if (i %% 2 != 0)
        {
          rowClass <- "odd"
        }
        else
        {
          rowClass <- "even"
        }
        layout[[i + 1]] <- paste0('<tr class="', rowClass, '"><td>', i, '</td><td>', itemStats()[i, 1], '</td><td>', itemStats()[i, 2], '</td><td><input id="check', i, '" type="checkbox" /></td></tr>')
      }
      HTML(c(layout, "</tbody></table>"))
    }
  })

  # observe the itemCutoffInput and update item checkboxes accordingly (on value change)
  observe ({
    input$itemCutoffInput
    if (!is.null(numberOfItems()) && !is.null(itemStats()) && !is.null(input$itemCutoffInput) && !is.na(input$itemCutoffInput))
    {
      for (i in 1:numberOfItems())
      {
        if (itemStats()[i, 2] < input$itemCutoffInput)
        {
          updateCheckboxInput(session, paste0("check", i), value=FALSE)
        }
        else
        {
          updateCheckboxInput(session, paste0("check", i), value=TRUE)
        }
      }
    }
  })
  
  # a vector of all unchecked item numbers (i.e. the items that should not be considered for the grading)
  outItems <- reactive ({
    if (!is.null(numberOfItems()) && !is.null(input$check1))
    {
      outItems <- c()
    
      for (i in 1:numberOfItems())
      {
        if (!eval(parse(text = paste0("input$check", i))))
        {
          outItems <- append(outItems, i)
        }
      }
      outItems
    }
  })
  
  # the whole dataset without the data of items that were not chosen
  allDataDef <- reactive ({
    if (length(outItems()) > 0)
    {
      # indexes of vector outItems is incremented by one because the first column of allData is the Matrikelnummer
      subset(allData(), select = -(outItems() + 1))
    }
    else
    {
      allData()
    }
  })
  
  # collect the maxPoints again, beacause the unchecked items were dropped
  maxPointsDefVec <- reactive ({
    if (!is.null(maxPointsValidatedVec()))
    {
      if (length(outItems()) > 0)
      {
        maxPointsValidatedVec()[-outItems()]
      }
      else
      {
        maxPointsValidatedVec()
      }
    }
  })
  
  # sum of maxPointsDefVec
  maxPointsDefSum <- reactive ({
    if (!is.null(maxPointsDefVec()))
    {
      sum(maxPointsDefVec())
    }
  })
  
  # find out how many points are needed for a grade 4, such that at least 80% of the students get a "pass"
  points4default <- reactive ({
    if (!is.null(allDataDef()) && !is.null(maxPointsDefSum()) && !is.null(numberOfValidExams))
    {
      value4 <- 0
      scores <- rowSums(allDataDef()[ , 2:ncol(allDataDef())])
    
      while (value4 <= maxPointsDefSum())
      { 
        slope <- calculateSlope(value4, 3.875, maxPointsDefSum(), 5.875)
        intercept <- calculateIntercept(3.875, slope, value4)
        
        grades <- sapply(scores, function(points)
        {
          round25(calculateY(slope, points, intercept))
        })
        if (sum(grades >= 4) / numberOfValidExams() < 0.8)
        {
          value4 <- value4 - 0.25
          break
        }
        value4 <- value4 + 0.25
      }
      value4
    }
  })
  
  # OUTPUT: slider input for the number of points for grade 4
  output$slider4 <- renderUI ({
    if (!is.null(maxPointsDefSum()) && !is.null(points4default()))
    {
      sliderInput("points4", label="Note 4", min=0, max=maxPointsDefSum(), value=points4default(), step=0.25, width="100%")
    }
    else
    {
      sliderInput("emptySlider", label="Note 4", min=0, max=100, value=0, step=0.25, width="100%")
    }
  })
  
  # OUTPUT: slider input for the number of points for grade 6
  output$slider6 <- renderUI ({
    if (!is.null(maxPointsDefSum()))
    {
      sliderInput("points6", label="Note 6", min=0, max=maxPointsDefSum(), value=maxPointsDefSum(), step=0.25, width="100%")
    }
    else
    {
      sliderInput("emptySlider", label="Note 6", min=0, max=100, value=100, step=0.25, width="100%")
    }
  })
  
  # the slope of the linear function for the grading
  slope <- reactive ({
    if (!is.null(input$points4) && !is.null(input$points6))
    {
      calculateSlope(input$points4, 3.875, input$points6, 5.875)
    }
  })
  
  # the intercept of the linear function for the grading
  intercept <- reactive ({
    if (!is.null(slope()) && !is.null(input$points4))
    {
      calculateIntercept(3.875, slope(), input$points4)
    }
  })
  
  # observe the slider input points4, such that points4 < points6 (with at minimum gap of 0.25)
  observe ({
    
    input$points4
    
    if (!is.null(input$points4) && !is.null(input$points6) && !is.null(maxPointsDefSum()))
    {
      if (input$points4 >= input$points6 && input$points6 != maxPointsDefSum())
      {
        updateSliderInput(session, "points6", label=NULL, value=input$points4 + 0.25)
      }
      else if (input$points4 >= input$points6)
      {
        updateSliderInput(session, "points4", label=NULL, value=input$points6 - 0.25)
      }
    }
  })
  
  # compute the final grades
  grades <- reactive ({
    if (!is.null(allDataDef()) && !is.null(slope()) && !is.null(intercept()))
    {
      matrikelnummern <- allDataDef()[ , 1]
      scores <- rowSums(allDataDef()[ , 2:ncol(allDataDef())])
      
      grades <- sapply(scores, function(points)
      {
        round25(calculateY(slope(), points, intercept()))
      })
      
      grades <- ifelse(grades > 6, 6, grades)
      grades <- ifelse(grades < 1, 1, grades)
      grades <- ifelse(scores == 0, 1, grades)
      
      temp <- cbind(matrikelnummern, scores, grades)
      colnames(temp) <- c("Matrikelnummer", "Punkte", "Note")
      
      temp
    }
  })
  
  # the number of passed exams
  numberOfPassed <- reactive ({
    if (!is.null(grades()))
    {
      sum(grades()[ , 3] >= 4)
    }
  })
  
  # percent of passed exam (invalid exams not considered)
  percentPassed <- reactive ({
    if (!is.null(numberOfPassed()) && !is.null(numberOfValidExams()))
    {
      numberOfPassed() / numberOfValidExams()
    }
  })
  
  # mean of all valid exam grades
  gradesMean <- reactive ({
    if (!is.null(grades()))
    {
      mean(subset(grades(), grades()[ , 2] != 0)[ , 3])
    }
  })
  
  # OUTPUT: html element to inform the user about the number and percent of passed exams
  output$text_passedInfo <- renderUI ({
    if (!is.null(numberOfPassed()) && !is.null(percentPassed()))
    {
      HTML(paste0(p("bestanden", style = "font-weight: bold"), numberOfPassed(), " (", round(percentPassed() * 100, 2), " %)"))
    }
    else
    {
      HTML(paste0(p("bestanden", style = "font-weight: bold"), 100, " (", round(100, 2), " %)"))
    }
  })
  
  # OUTPUT: html element to inform the user about the number and percent of failed exams
  output$text_failedInfo <- renderUI ({
    if (!is.null(numberOfValidExams()) && !is.null(numberOfPassed()) && !is.null(percentPassed()))
    {
      HTML(paste0(p("nicht bestanden", style = "font-weight: bold"), numberOfValidExams() - numberOfPassed(), " (", round((1 - percentPassed()) * 100, 2), " %)"))
    }
    else
    {
      HTML(paste0(p("nicht bestanden", style = "font-weight: bold"), 0, " (", round(0, 2), " %)"))
    }
  })
  
  # OUTPUT: html element to inform the user about the mean of the grades (only valid exams considered)
  output$text_meanInfo <- renderUI ({
    if (!is.null(numberOfValidExams()) && !is.null(numberOfPassed()) && !is.null(percentPassed()))
    {
      HTML(paste0(p("Notendurchschnitt", style = "font-weight: bold"), round(gradesMean(), 2)))
    }
    else
    {
      HTML(paste0(p("Notendurchschnitt", style = "font-weight: bold"), round(6, 2)))
    }
  })
  
  # the grade that was reached the most often
  modalWert <- reactive ({
    if (!is.null(grades()))
    {
      max(sort(table(grades()[ , 3]), decreasing=TRUE))
    }
  })
  
  # OUTPUT: a plot to show the linear function for the grading
  output$curve_function <- renderPlot ({
    if (!is.null(slope()) && !is.null(intercept()) && !is.null(intercept()) && !is.null(maxPointsDefSum()) && !is.null(input$points4) && !is.null(input$points6))
    {
      curve(slope() * x + intercept(), 0, maxPointsDefSum(), xlim=c(0, maxPointsDefSum()), ylim=c(1, 6), type="n", xlab="Anzahl Punkte", ylab="Note", main="Funktion der Notengebung")
      polygon(c(0, 0, input$points4, input$points4), c(1, intercept(), 3.875, 1), col="red", border=FALSE)
      polygon(c(input$points4, input$points4, maxPointsDefSum(), maxPointsDefSum()), c(1, 3.875, slope() * maxPointsDefSum() + intercept(), 1), col="green", border=FALSE)
    }
  })
  
  # OUTPUT: a histogram to visualize the distribution of the final grades
  output$hist_grades <- renderPlot ({
    if (!is.null(grades()) && !is.null(modalWert()))
    {
      h <- hist(grades()[ , 3], breaks=seq(0.875,6.125,0.25))
      c = ifelse(h$mids < 4, "red", "green");
      plot(h, main="Verteilung der Noten", xlab="Note", ylab="Häufigkeit", xlim=c(0.75,6.25), ylim=c(0,modalWert()+10), col=c, border="white", labels=TRUE)
    }
  })
  
  # OUTPUT: the final grades table
  output$table_grades <- renderDataTable ({
    if (!is.null(grades()))
    {
      grades()
    }
  })
  
  observe ({
    if (!is.null(grades()))
    {
      
      # OUTPUT: the download handler for the grades table (.csv)
      output$csv <- downloadHandler (
        filename = "Notentabelle.csv",
        content = function(file)
        {
          write.csv(grades(), file, row.names = FALSE, fileEncoding="UTF-8")
        }
      )
      
      # OUTPUT: the download handler for the report (.pdf)
      output$report = downloadHandler (
        filename = "Auswertungsbericht.pdf",
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
      
    }
  })
  
})
