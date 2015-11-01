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

# returns a value rounded to halfs
round5 <- function (n)
{
  return (round(n / 0.5) * 0.5)
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
  
#   # number of students to take the exam
#   numberOfStudents <- reactive ({
#     if (!is.null(allData()))
#     {
#       nrow(allData())
#     }
#   })
#   
#   # number of invalid exams (points achieved == 0)
#   numberOfInvalidExams <- reactive ({
#     if (!is.null(allData()))
#     {
#       sum(rowSums(allData()[ , 2:ncol(allData())]) == 0)
#     }
#   })
#   
#   # number of valid exams (point achieved > 0)
#   numberOfValidExams <- reactive ({
#     if (!is.null(numberOfStudents()) && !is.null(numberOfInvalidExams()))
#     {
#       numberOfStudents() - numberOfInvalidExams()
#     }
#   })
#   
#   # number of items
#   numberOfItems <- reactive ({
#     if (!is.null(allData()))
#     {
#       ncol(allData()) - 1
#     }
#   })
#   
#   # OUTPUT: html element with the number of students found
#   output$text_numberOfStudents <- renderUI ({
#     if (!is.null(numberOfStudents()))
#     {
#       HTML(paste0(p("Anzahl Studenten", style = "font-weight: bold"), numberOfStudents()))
#     }
#   })
#   
#   # OUTPUT: html element with the number of items found
#   output$text_numberOfItems <- renderUI ({
#     if (!is.null(numberOfItems()))
#     {
#       HTML(paste0(p("Anzahl Items", style = "font-weight: bold"), numberOfItems()))
#     }
#   })
#   
#   # allData without the column for the Matrikelnummer (gives us just the item data, depending on the decision if empty tests should be excluded or not)
#   itemData <- reactive ({
#     if (!is.null(allData()))
#     {
#       if (input$emptyTestsDecision == "Ja")
#       {
#         allData()[rowSums(allData()[ , 2:ncol(allData())])!=0, ][ , 2:ncol(allData())]
#       }
#       else
#       {
#         allData()[ , 2:ncol(allData())]
#       }
#     }
#   })
#   
#   # vector with a guess (based on the input data) about type for each item (sc or mc)
#   itemTypesVec <- reactive ({
#     if (!is.null(rawData()))
#     {
#       temp <- dcast(rawData(), eval(parse(text = colnames(rawData())[1])) ~ eval(parse(text = colnames(rawData())[3])), value.var = colnames(rawData())[ncol(rawData()) - 1])
#       temp <- temp[-1]
#       temp <- colSums(temp)
#       temp <- ifelse(temp > 0, '<option value="sc" selected>Single Choice</option><option value="mc">Multiple Choice</option>', '<option value="sc">Single Choice</option><option value="mc" selected>Multiple Choice</option>')
#     }
#     temp
#   })
#   
#   # vector with a guess (based on the input data) about the number of alternatives for each item
#   itemAlternGuessVec <- reactive ({
#     if (!is.null(rawData()))
#     {
#       rtn <- rep(0, numberOfItems())
#       for (j in 1:(ncol(rawData()) - 4))
#       {
#         temp <- dcast(rawData(), eval(parse(text = colnames(rawData())[1])) ~ eval(parse(text = colnames(rawData())[3])), value.var = colnames(rawData())[ncol(rawData()) - j])
#         temp <- temp[-1]
#         temp <- colSums(temp)
#         temp <- ifelse(temp != 0, ncol(rawData()) - 3 - j, 0)
#         rtn <- ifelse(rtn > 0, rtn, temp)
#         if (as.logical(all(rtn) > 0))
#         {
#           break
#         }
#       }
#     }
#     rtn
#   })
#   
#   # vector with a guess (based on the input data) about the maximum points for each item
#   maxPointsGuessVec <- reactive ({
#     if (!is.null(itemData()))
#     {
#       apply(itemData(), 2, max)
#     }
#   })
#   
#   # OUTPUT: list of numeric inputs, default values are the guessed points for each question
#   output$numericInputs <- renderUI ({
#     if (!is.null(numberOfItems()) && !is.null(maxPointsGuessVec()) && !is.null(itemTypesVec()))
#     {
#       numericInputs <- list()
#       numericInputs[[1]] <- ('
#         <table class="table table-striped table-hover">
#           <thead>
#             <tr>
#               <th>
#                 Item
#               </th>
#               <th>
#                 Typ
#               </th>
#               <th>
#                 Anzahl Alternativen
#               </th>
#               <th>
#                 Maximal erreichbare Punkte
#               </th>
#             </tr>
#           </thead>
#           <tfoot>
#             <tr>
#               <th>
#                 Item
#               </th>
#               <th>
#                 Typ
#               </th>
#               <th>
#                 Anzahl Alternativen
#               </th>
#               <th>
#                 Maximal erreichbare Punkte
#               </th>
#             </tr>
#           </tfoot>
#           <tbody>
#       ')
#       
#       for (i in 1:numberOfItems())
#       {
#         numericInputs[[i + 1]] <- paste0('
#         <tr>
#           <td width="10%">Frage ',
#             i,'
#           </td>
#           <td>
#             <div class="form-group shiny-input-container">
#               <div>
#                 <select id="type', i, '">',
#                   itemTypesVec()[[i]],'
#                 </select>
#                 <script type="application/json" data-for="type', i, '" data-nonempty="">{}</script>
#               </div>
#             </div>
#           </td>
#           <td>
#             <div class="form-group shiny-input-container">
#               <input id="', paste0("altern", i), '" type="number" class="form-control" value="', itemAlternGuessVec()[[i]], '" step="1"/>
#             </div>
#           </td>
#           <td>
#             <div class="form-group shiny-input-container">
#               <input id="', paste0("max", i), '" type="number" class="form-control" value="', maxPointsGuessVec()[[i]], '" step="1"/>
#             </div>
#           </td>
#         </tr>
#         ')
#       }
#       HTML(c(numericInputs, "</tbody></table>"))
#     }
#   })
#   
#   # vector of the user validated item type for each item
#   itemTypesValidatedVec <- reactive ({
#     if (!is.null(numberOfItems()) && !is.null(input$type1))
#     {
#       checkNA <- TRUE
#       
#       for (i in 1:numberOfItems())
#       {
#         if (is.na(eval(parse(text = paste0('input$type', i)))))
#         {
#           checkNA <- FALSE
#           break
#         }
#       }
#       
#       if (checkNA)
#       {
#         temp <- c()
#         for (i in 1:numberOfItems())
#         {
#           temp <- append(temp, eval(parse(text = paste0('input$type', i))))
#         }
#         temp
#       }
#     }
#   })
#   
#   # vector of the user validated number of alternatives for each item
#   itemAlternValidatedVec <- reactive ({
#     if (!is.null(numberOfItems()) && !is.null(input$altern1))
#     {
#       checkNA <- TRUE
#       
#       for (i in 1:numberOfItems())
#       {
#         if (is.na(eval(parse(text = paste0('input$altern', i)))))
#         {
#           checkNA <- FALSE
#           break
#         }
#       }
#       
#       if (checkNA)
#       {
#         temp <- c()
#         for (i in 1:numberOfItems())
#         {
#           temp <- append(temp, eval(parse(text = paste0('input$altern', i))))
#         }
#         temp
#       }
#     }
#   })
#   
#   # vector of the user validated maximum points for each item
#   maxPointsValidatedVec <- reactive ({
#     if (!is.null(numberOfItems()) && !is.null(input$max1))
#     {
#       checkNA <- TRUE
#       
#       for (i in 1:numberOfItems())
#       {
#         if (is.na(eval(parse(text = paste0('input$max', i)))))
#         {
#           checkNA <- FALSE
#           break
#         }
#       }
#       
#       if (checkNA)
#       {
#         temp <- c()
#         for (i in 1:numberOfItems())
#         {
#           temp <- append(temp, eval(parse(text = paste0('input$max', i))))
#         }
#         temp
#       }
#     }
#   })
#   
#   # sum of the user validated maximum points for each question
#   maxPointsValidatedSum <- reactive ({
#     if (!is.null(maxPointsValidatedVec()))
#     {
#       sum(maxPointsValidatedVec())
#     }
#   })
#   
#   # OUTPUT: html element with the number of invalid exams
#   output$text_numberOfEmptyTests <- renderUI ({
#     if (!is.null(numberOfInvalidExams()))
#     {
#       HTML(paste0(p("Anzahl leere Pr??fungen ", style = "font-weight: bold"), numberOfInvalidExams()))
#     }
#   })
#   
#   # OUTPUT: plot of the scores' distribution (depending on the deciision if a histogram or a qq plot should be shown)
#   output$plot_distribution <- renderPlot ({
#     if (!is.null(itemData()) && !is.null(maxPointsValidatedSum()))
#     {
#       if (input$distributionPlotDecision == "Histogramm")
#       {
#         h <- hist(rowSums(itemData()), main="Verteilung der Punkte", xlab="Anzahl Punkte", ylab="H??ufigkeit", col = "lightblue", breaks = maxPointsValidatedSum()/5, xlim=c(0, maxPointsValidatedSum()), xaxt='n')
#         axis(1, at=seq(0, maxPointsValidatedSum(), 5))
#         xfit <- seq(0, maxPointsValidatedSum(), length=maxPointsValidatedSum())
#         yfit <- dnorm(xfit, mean=mean(rowSums(itemData())), sd=sd(rowSums(itemData())))
#         yfit <- yfit*diff(h$mids[1:2])*length(rowSums(itemData()))
#         lines(xfit, yfit)
#         abline(v = mean(rowSums(itemData())), col = "blue", lwd = 1)
#         abline(v = median(rowSums(itemData())), col = "orange", lwd = 1)
#         legend("topright", legend = c("Mittelwert", "Median"), col = c("blue", "orange"), lwd=1)
#       }
#       else
#       {
#         qqnorm(rowSums(itemData()), main="Normaler Q-Q-Plot", xlab="Erwartete Quantile", ylab="Beobachtete Quantile")
#         qqline(rowSums(itemData()), col=2)
#       }
#     }
#   })
#   
#   # two item statistics: item difficulty and item total correlation
#   itemStats <- reactive ({
#     if (!is.null(itemData()) && !is.null(maxPointsValidatedVec()))
#     {
#       cbind(itemDifficulties(itemData(), maxPointsValidatedVec()), itemTotalCorrelations(itemData()))
#     }
#   })
#   
#   # two item statistics for every subitem: item difficulty and item total correlation
#   subitemStats <- reactive ({
#     itemTotalCorrelations(points_a())
#   })
#   
#   # the default (starting) cut off value for the item total correlation (chosen such that the two items are below)
#   itemCutoffDefault <- reactive ({
#     if (!is.null(itemStats()))
#     {
#       temp <- itemStats()[order(itemStats()[ , 2]), ][2, 2] + 0.01
#       if (temp > 1)
#       {
#         temp <- 1
#       }
#       else if (temp < -1)
#       {
#         temp <- -1
#       }
#       temp
#     }
#   })
#   
#   # OUTPUT: numeric input to adjust the cutoff value for the item total correlation
#   output$itemCutoffInput <- renderUI ({
#     if (!is.null(itemCutoffDefault()))
#     {
#       numericInput("itemCutoffInput", "Legen Sie den Mindestwert f??r die Trennsch??rfe fest.", min=-1, max=1, step=0.01, value=itemCutoffDefault())
#     }
#     else
#     {
#       numericInput("itemCutoffInput", "Legen Sie den Mindestwert f??r die Trennsch??rfe fest.", min=-1, max=1, step=0.01, value=0)
#     }
#   })
#   
#   output$itemStatsLayout <- renderUI ({
#     
#     stats <- subitemStats()
#     
#     if (!is.null(numberOfItems()) && !is.null(itemStats()))
#     {
#       layout <- list()
#       layout[[1]] <- ('<table class="table table-striped table-hover", id="itemStatsTable"><thead><tr><th>Item</th><th>Schwierigkeit</th><th>Trennsch??rfe</th><th></th></tr></thead><tbody>')
#       
#       for (i in 1:numberOfItems())
#       {
#         if (itemTypesValidatedVec()[[i]] == "sc")
#         {
#           layout[[i + 1]] <- paste0('<tr><td>', i, '</td><td>', itemStats()[i, 1], '</td><td>', itemStats()[i, 2], '</td><td><input id="check', i, '" type="checkbox" /></td></tr>')
#           stats <- stats[-c(1:itemAlternValidatedVec()[[i]])]
#         }
#         else
#         {
#           temp <- c()
#           for (j in 1:itemAlternValidatedVec()[[i]])
#           {
#             temp <- append(temp, paste0('<tr style="font-weight: lighter">
#               <td>', i, '.', j, '</td>
#               <td>
#                 <div>
#                 <div id="1" class="shiny-plot-output" style="width: 200px">
#                 </div>
#                 </div>
#               </td>
#               <td>',
#                 stats[[1]]
#               ,'</td>
#               <td>
#                 <input id="subcheck', i, j, '" type="checkbox" checked/>
#               </td>
#             </tr>'))
#             stats <- stats[-1]
#           }
#           layout[[i + 1]] <- paste0('<tr><td>', i, '</td><td>', itemStats()[i, 1], '</td><td>', itemStats()[i, 2], '</td><td><input id="check', i, '" type="checkbox" /></td></tr>', paste0(temp, collapse = ""))
#         }
#       }
#       HTML(c(layout, "</tbody></table>"))
#     }
#   })
#   
#   # observe the itemCutoffInput and update item checkboxes accordingly (on value change)
#   observe ({
#     input$itemCutoffInput
#     if (!is.null(numberOfItems()) && !is.null(itemStats()) && !is.null(input$itemCutoffInput) && !is.na(input$itemCutoffInput))
#     {
#       for (i in 1:numberOfItems())
#       {
#         if (itemStats()[i, 2] < input$itemCutoffInput)
#         {
#           updateCheckboxInput(session, paste0("check", i), value=FALSE)
#         }
#         else
#         {
#           updateCheckboxInput(session, paste0("check", i), value=TRUE)
#         }
#       }
#     }
#   })
#   
#   # a vector of all unchecked item numbers (i.e. the items that should not be considered for the grading)
#   outItems <- reactive ({
#     if (!is.null(numberOfItems()) && !is.null(input$check1))
#     {
#       outItems <- c()
#     
#       for (i in 1:numberOfItems())
#       {
#         if (!eval(parse(text = paste0("input$check", i))))
#         {
#           outItems <- append(outItems, i)
#         }
#       }
#       outItems
#     }
#   })
#   
#   # the whole dataset without the data of items that were not chosen
#   allDataDef <- reactive ({
#     if (length(outItems()) > 0)
#     {
#       # indexes of vector outItems is incremented by one because the first column of allData is the Matrikelnummer
#       subset(allData(), select = -(outItems() + 1))
#     }
#     else
#     {
#       allData()
#     }
#   })
#   
#   # collect the maxPoints again, beacause the unchecked items were dropped
#   maxPointsDefVec <- reactive ({
#     if (!is.null(maxPointsValidatedVec()))
#     {
#       if (length(outItems()) > 0)
#       {
#         maxPointsValidatedVec()[-outItems()]
#       }
#       else
#       {
#         maxPointsValidatedVec()
#       }
#     }
#   })
#   
#   # sum of maxPointsDefVec
#   maxPointsDefSum <- reactive ({
#     if (!is.null(maxPointsDefVec()))
#     {
#       sum(maxPointsDefVec())
#     }
#   })
#   
#   # find out how many points are needed for a grade 4, such that at least 80% of the students get a "pass"
#   points4default <- reactive ({
#     if (!is.null(allDataDef()) && !is.null(maxPointsDefSum()) && !is.null(numberOfValidExams))
#     {
#       value4 <- 0
#       scores <- rowSums(allDataDef()[ , 2:ncol(allDataDef())])
#     
#       while (value4 <= maxPointsDefSum())
#       { 
#         slope <- calculateSlope(value4, 3.875, maxPointsDefSum(), 5.875)
#         intercept <- calculateIntercept(3.875, slope, value4)
#         
#         grades <- sapply(scores, function(points)
#         {
#           round25(calculateY(slope, points, intercept))
#         })
#         if (sum(grades >= 4) / numberOfValidExams() < 0.8)
#         {
#           value4 <- value4 - 0.25
#           break
#         }
#         value4 <- value4 + 0.25
#       }
#       value4
#     }
#   })
#   
#   # OUTPUT: slider input for the number of points for grade 4
#   output$slider4 <- renderUI ({
#     if (!is.null(maxPointsDefSum()) && !is.null(points4default()))
#     {
#       sliderInput("points4", label="Note 4", min=0, max=maxPointsDefSum(), value=points4default(), step=0.25, width="100%")
#     }
#     else
#     {
#       sliderInput("emptySlider", label="Note 4", min=0, max=100, value=0, step=0.25, width="100%")
#     }
#   })
#   
#   # OUTPUT: slider input for the number of points for grade 6
#   output$slider6 <- renderUI ({
#     if (!is.null(maxPointsDefSum()))
#     {
#       sliderInput("points6", label="Note 6", min=0, max=maxPointsDefSum(), value=maxPointsDefSum(), step=0.25, width="100%")
#     }
#     else
#     {
#       sliderInput("emptySlider", label="Note 6", min=0, max=100, value=100, step=0.25, width="100%")
#     }
#   })
#   
#   # the slope of the linear function for the grading
#   slope <- reactive ({
#     if (!is.null(input$points4) && !is.null(input$points6))
#     {
#       calculateSlope(input$points4, 3.875, input$points6, 5.875)
#     }
#   })
#   
#   # the intercept of the linear function for the grading
#   intercept <- reactive ({
#     if (!is.null(slope()) && !is.null(input$points4))
#     {
#       calculateIntercept(3.875, slope(), input$points4)
#     }
#   })
#   
#   # observe the slider input points4, such that points4 < points6 (with at minimum gap of 0.25)
#   observe ({
#     
#     input$points4
#     
#     if (!is.null(input$points4) && !is.null(input$points6) && !is.null(maxPointsDefSum()))
#     {
#       if (input$points4 >= input$points6 && input$points6 != maxPointsDefSum())
#       {
#         updateSliderInput(session, "points6", label=NULL, value=input$points4 + 0.25)
#       }
#       else if (input$points4 >= input$points6)
#       {
#         updateSliderInput(session, "points4", label=NULL, value=input$points6 - 0.25)
#       }
#     }
#   })
#   
#   # compute the final grades
#   grades <- reactive ({
#     if (!is.null(allDataDef()) && !is.null(slope()) && !is.null(intercept()))
#     {
#       matrikelnummern <- allDataDef()[ , 1]
#       scores <- rowSums(allDataDef()[ , 2:ncol(allDataDef())])
#       
#       grades <- sapply(scores, function(points)
#       {
#         round25(calculateY(slope(), points, intercept()))
#       })
#       
#       grades <- ifelse(grades > 6, 6, grades)
#       grades <- ifelse(grades < 1, 1, grades)
#       grades <- ifelse(scores == 0, 1, grades)
#       
#       temp <- cbind(matrikelnummern, scores, grades)
#       colnames(temp) <- c("Matrikelnummer", "Punkte", "Note")
#       
#       temp
#     }
#   })
#   
#   # the number of passed exams
#   numberOfPassed <- reactive ({
#     if (!is.null(grades()))
#     {
#       sum(grades()[ , 3] >= 4)
#     }
#   })
#   
#   # percent of passed exam (invalid exams not considered)
#   percentPassed <- reactive ({
#     if (!is.null(numberOfPassed()) && !is.null(numberOfValidExams()))
#     {
#       numberOfPassed() / numberOfValidExams()
#     }
#   })
#   
#   # mean of all valid exam grades
#   gradesMean <- reactive ({
#     if (!is.null(grades()))
#     {
#       mean(subset(grades(), grades()[ , 2] != 0)[ , 3])
#     }
#   })
#   
#   # OUTPUT: html element to inform the user about the number and percent of passed exams
#   output$text_passedInfo <- renderUI ({
#     if (!is.null(numberOfPassed()) && !is.null(percentPassed()))
#     {
#       HTML(paste0(p("bestanden", style = "font-weight: bold"), numberOfPassed(), " (", round(percentPassed() * 100, 2), " %)"))
#     }
#     else
#     {
#       HTML(paste0(p("bestanden", style = "font-weight: bold"), 100, " (", round(100, 2), " %)"))
#     }
#   })
#   
#   # OUTPUT: html element to inform the user about the number and percent of failed exams
#   output$text_failedInfo <- renderUI ({
#     if (!is.null(numberOfValidExams()) && !is.null(numberOfPassed()) && !is.null(percentPassed()))
#     {
#       HTML(paste0(p("nicht bestanden", style = "font-weight: bold"), numberOfValidExams() - numberOfPassed(), " (", round((1 - percentPassed()) * 100, 2), " %)"))
#     }
#     else
#     {
#       HTML(paste0(p("nicht bestanden", style = "font-weight: bold"), 0, " (", round(0, 2), " %)"))
#     }
#   })
#   
#   # OUTPUT: html element to inform the user about the mean of the grades (only valid exams considered)
#   output$text_meanInfo <- renderUI ({
#     if (!is.null(numberOfValidExams()) && !is.null(numberOfPassed()) && !is.null(percentPassed()))
#     {
#       HTML(paste0(p("Notendurchschnitt", style = "font-weight: bold"), round(gradesMean(), 2)))
#     }
#     else
#     {
#       HTML(paste0(p("Notendurchschnitt", style = "font-weight: bold"), round(6, 2)))
#     }
#   })
#   
#   # the grade that was reached the most often
#   modalWert <- reactive ({
#     if (!is.null(grades()))
#     {
#       max(sort(table(grades()[ , 3]), decreasing=TRUE))
#     }
#   })
#   
#   # OUTPUT: a plot to show the linear function for the grading
#   output$curve_function <- renderPlot ({
#     if (!is.null(slope()) && !is.null(intercept()) && !is.null(intercept()) && !is.null(maxPointsDefSum()) && !is.null(input$points4) && !is.null(input$points6))
#     {
#       curve(slope() * x + intercept(), 0, maxPointsDefSum(), xlim=c(0, maxPointsDefSum()), ylim=c(1, 6), type="n", xlab="Anzahl Punkte", ylab="Note", main="Funktion der Notengebung")
#       polygon(c(0, 0, input$points4, input$points4), c(1, intercept(), 3.875, 1), col="red", border=FALSE)
#       polygon(c(input$points4, input$points4, maxPointsDefSum(), maxPointsDefSum()), c(1, 3.875, slope() * maxPointsDefSum() + intercept(), 1), col="green", border=FALSE)
#     }
#   })
#   
#   # OUTPUT: a histogram to visualize the distribution of the final grades
#   output$hist_grades <- renderPlot ({
#     if (!is.null(grades()) && !is.null(modalWert()))
#     {
#       h <- hist(grades()[ , 3], breaks=seq(0.875,6.125,0.25))
#       c = ifelse(h$mids < 4, "red", "green");
#       plot(h, main="Verteilung der Noten", xlab="Note", ylab="H??ufigkeit", xlim=c(0.75,6.25), ylim=c(0,modalWert()+10), col=c, border="white", labels=TRUE)
#     }
#   })
#   
#   # OUTPUT: the final grades table
#   output$table_grades <- renderDataTable ({
#     if (!is.null(grades()))
#     {
#       grades()
#     }
#   })
#   
#   observe ({
#     if (!is.null(grades()))
#     {
#       
#       # OUTPUT: the download handler for the grades table (.csv)
#       output$csv <- downloadHandler (
#         filename = "Notentabelle.csv",
#         content = function(file)
#         {
#           write.csv(grades(), file, row.names = FALSE, fileEncoding="UTF-8")
#         }
#       )
#       
#       # OUTPUT: the download handler for the report (.pdf)
#       output$report = downloadHandler (
#         filename = "Auswertungsbericht.pdf",
#         content = function(file)
#         {
#           out = knit2pdf('template.rnw', clean = TRUE, encoding="UTF-8")
#           file.rename(out, file) # move pdf to file for downloading
#           file.remove("template.tex")
#           file.remove("figure/figure1-1.pdf", "figure/figure2-1.pdf", "figure/figure3-1.pdf", "figure/figure4-1.pdf")
#           file.remove("figure")
#         },
#         contentType = 'application/pdf'
#       )
#       
#     }
#   })
#   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW
  # NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW
  # NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW
  # NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW NEW
  
  # read csv
  data <- reactive ({
    if (!is.null(input$inputFile))
    {
      read.csv(input$inputFile$datapath)
    }
  })
  
  # number of items
  numberOfItems <- reactive ({
    if (!is.null(data()))
    {
      max(data()$Fragennummer)
    }
  })
  
  # number of students
  numberOfStudents <- reactive ({
    if (!is.null(data()) & !is.null(numberOfItems()))
    {
      nrow(data()) %/% numberOfItems()
    }
  })
  
  # OUTPUT: confirmation text
  output$confirmation <- renderUI ({
    if (!is.null(numberOfItems()) & !is.null(numberOfStudents()))
    {
      if (numberOfItems() > 0 & numberOfStudents() > 0)
      {
        HTML(paste0('<font color = "green">Data found: <b>', numberOfStudents(), '</b> students and <b>', numberOfItems(), '</b> items.</font>'))
      }
      else
      {
        HTML('<font color = "red">Something went wrong. Please check settings for header and separator.</font>')
      }
    }
    else
    {
      HTML("No data uploaded yet.")
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
  
  # guess item types
  itemTypes <- reactive ({
    if (!is.null(data()))
    {
      rep(ifelse(rowSums(aggregate(. ~ Fragennummer, data(), function(x) -1 %in% x)[4:8]) > 0, "Multiple Choice", "Single Choice"), each = 5)
    }
  })
  
  # guess max. points/item
  itemPointsGuess <- reactive ({
    if (!is.null(data()))
    {
      rep(aggregate(Punkte ~ Fragennummer, data(), max)$Punkte, each = 5)
    }
  })
  
  # guess nr. of alternatives for each item
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
  
  # OUTPUT: table to validate guessed information !!!!!!!!!!
  output$validationTable <- renderUI ({
    
    index <- seq(1, 160, 5)
    
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
    
  })
  
  itemPoints <- reactive ({
    temp <- c()
    for (i in 1:numberOfItems())
    {
      temp <- c(temp, eval(parse(text = paste0('input$itemPoints', i))))
    }
    rep(temp, each = 5)
  })
  
  solution <- reactive ({
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
  })
  
  corresponding <- reactive ({
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
  })
  
  itemData <- reactive ({
    if (!is.null(data()))
    {
      temp <- dcast(data(), data()$liebmannnr ~ data()$Fragennummer, value.var = "Punkte")[-1]
      temp[rowSums(temp)!= 0, ]
    }
  })
  
  # OUTPUT: histogram of raw scores
  output$histogramRawScores <- renderPlot ({
    totalPoints <- sum(itemPoints()) / 5
    h <- hist(rowSums(itemData()), main="Distribution of raw scores", xlab="Points", ylab="Frequency", col = "lightblue", breaks = totalPoints/5, xlim=c(0, totalPoints), xaxt='n')
    axis(1, at=seq(0, totalPoints, 5))
    xfit <- seq(0, totalPoints, length=totalPoints)
    yfit <- dnorm(xfit, mean=mean(rowSums(itemData())), sd=sd(rowSums(itemData())))
    yfit <- yfit*diff(h$mids[1:2])*length(rowSums(itemData()))
    lines(xfit, yfit)
    abline(v = mean(rowSums(itemData())), col = "blue", lwd = 1)
    abline(v = median(rowSums(itemData())), col = "orange", lwd = 1)
    legend("topright", legend = c("Mean", "Median"), col = c("blue", "orange"), lwd=1)
  })
  
  # OUTPUT: q-q plot of raw scores
  output$qqRawScores <- renderPlot ({
    qqnorm(rowSums(itemData()), main="Normal Q-Q plot", xlab="Expected Quantiles", ylab="Observed Quantiles")
    qqline(rowSums(itemData()), col=2)
  })
  
  # reorder data
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
          <tr style = "background: #F5F5F5; height: 80px">
            <td style="vertical-align: middle">
              ', i, '
            </td>
            <td style="vertical-align: middle; padding-left: 15px">
              ', itemDifficulties()[i], '
            </td>
            <td style="vertical-align: middle;">
              ', itemTotalCorrelations()[i], '
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
          <tr style = "background: #F5F5F5; height: 80px">
            <td style="vertical-align: middle">
              ', i, '
            </td>
            <td style="vertical-align: middle; padding-left: 15px">
              ', itemDifficulties()[i], '
            </td>
            <td style="vertical-align: middle;">
              ', itemTotalCorrelations()[i], '
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
    print(class(temp))
    temp
  })
  
  # update correctness
  correctnessFinal <- reactive ({
    temp <- correctness()
    temp[outItems()] <- 1
  })
  
  # total scores
  scores <- reactive ({
    
    pointsPerAlternative <- itemPoints() / itemAlternatives()
    scores <- data.frame(mapply('*', correctnessFinal(), pointsPerAlternative), row.names = rownames(correctnessFinal()))
    
    #index <- seq(1, 160, 5)
    #scoresRounded <- sapply(index, function(x) ifelse(itemTypes()[x] == "Single Choice" & rowSums(scores[ , x:(x + 4)]) != itemPoints()[x], 0, round25(rowSums(scores[ , x:(x + 4)]))))
    #scoresRounded <- ifelse(scoresRounded < 0, 0, scoresRounded)
    scores
  })
  
  # slider 4
  output$slider4 <- renderUI ({
    sliderInput("points4", label="Grade 4", min=0, max=sum(itemPoints()) / 5, value=sum(itemPoints()) / 5 / 2, step=0.25)
  })
  
  # slider 6
  output$slider6 <- renderUI ({
    sliderInput("points6", label="Grade 6", min=0, max=sum(itemPoints()) / 5, value=sum(itemPoints()) / 5, step=0.25)
  })
  
  # grading slope
  slope <- reactive ({
    if (!is.null(input$points4) && !is.null(input$points6))
    {
      calculateSlope(input$points4, 3.875, input$points6, 5.875)
    }
  })
  
  output$testo <- renderDataTable({
    correctnessFinal()
  })
  
  output$blap <- renderText ({
    slope()
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