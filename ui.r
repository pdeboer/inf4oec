library(shiny)

shinyUI(navbarPage("Auswertung inf4oec", id = "main", inverse = TRUE, collapsable = TRUE, footer=tags$br(),
  
  tabPanel("Information",
    fluidPage(
      fluidRow(
        column(12,
          h1("Willkommen", style = "color: lightblue"),
          p("Diese Applikation hilft Ihnen, die Prüfung zur Vorlesung", em("Informatik für Ökonomen I"), "effizient und übersichtlich auszuwerten."),
          tags$hr(),
          actionButton("next1", h4("zum ersten Schritt", style = "color: lightblue"))
        )
      )
    )
  ),
  
  navbarMenu("Daten",
    tabPanel("Schritt 1: Daten einlesen",
      fluidPage(
        fluidRow(
          column(12,
            h1("Schritt 1: Daten einlesen", style = "color: lightblue"),
            p('Wählen Sie eine CSV-Datei mit den Spalten "Matrikelnummer", "Serie", "Fragennummer", Alternativen ("A", "B", "C" etc.) und "Punkte" aus.'),
            p("Prüfen Sie, ob die Datei korrekt eingelesen wurde. Passen Sie anderenfalls die Einstellungen in der Seitenleiste links an.", tags$br(), "Bitte beachten Sie, dass die Daten nach diesem Schritt nur weiterverarbeitet werden, wenn die Anzahl Fragen > 1 ist."),
            tags$hr()
          )
        ),
        fluidRow(
          column(4,
            fileInput('inputFile', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            checkboxInput('header', 'Header', TRUE),
            tags$br(),
            radioButtons('sep', tags$b('Separator'), c(Comma=',', Semicolon=';', Tab='\t'), ','),
            tags$br(),
            radioButtons('quote', tags$b('Quote'), c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"'),
            tags$hr(),
            fluidRow(
              column(6,
                actionButton("back0", h4("zurück zum Anfang", style = "color: lightgray"))
              ),
              column(6,
                actionButton("next2", h4("weiter zu Schritt 2", style = "color: lightblue"))
              )
            )
          ),
          column(8,
            dataTableOutput("table_rawData")
          )
        )
      )
    ),
    tabPanel("Schritt 2: Daten transponieren",
      fluidPage(
        fluidRow(
          column(12,
            h1("Schritt 2: Daten transponieren", style = "color: lightblue"),
            p("Prüfen Sie aufgrund der Anzahl Studenten/Items, ob die Daten korrekt transponiert wurden."),
            p("Stellen Sie anderenfalls sicher, dass die eingelesene Datei die unter Schritt 1 beschriebene Strukutr aufweist."),
            tags$hr()
          )
        ),
        fluidRow(
          column(4,
            fluidRow(
              column(6,
                actionButton("back1", h4("zurück zu Schritt 1", style = "color: lightgray"))
              ),
              column(6,
                actionButton("next3", h4("weiter zu Schritt 3", style = "color: lightblue"))
              )
            )
          ),
          column(8,
            column(6,
              uiOutput("text_numberOfStudents")
            ),
            column(6,
              uiOutput("text_numberOfItems")
            )
          )
        )
      )
    ),
    tabPanel("Schritt 3: Maximal erreichbare Punkte",
      fluidPage(
        fluidRow(
          column(12,
            h1("Schritt 3: Maximal erreichbare Punkte", style = "color: lightblue"),
            p("Prüfen Sie, ob die maximal erreichbaren Punkte für jedes Item korrekt ausgelesen wurden."),
            p("Passen Sie anderenfalls die Anzahl Punkte manuell an."),
            tags$hr()
          )
        ),
        fluidRow(
          column(4,
            fluidRow(
              column(6,
                actionButton("back2", h4("zurück zu Schritt 2", style = "color: lightgray"))
              ),
              column(6,
                actionButton("next4", h4("weiter zu Schritt 4", style = "color: lightblue"))
              )
            )
          ),
          column(8,
            uiOutput("numericInputs")
          )
        )
      )
    )
  ),
  
  navbarMenu("Itemanalyse",
    tabPanel("Schritt 4: Leere Prüfungen",
      fluidPage(
        fluidRow(
          column(12,
            h1("Schritt 4: Leere Prüfungen", style = "color: lightblue"),
            p("Es wird empfohlen, leere Prüfungen für die Itemanalyse auszuschliessen."),
            tags$hr()
          )
        ),
        fluidRow(
          column(4,
            selectInput("emptyTestsDecision", label = "Leere Prüfungen ausschliessen?", choices = list("Ja", "Nein"), selected = "Ja"),
            tags$hr(),
            fluidRow(
              column(6,
                actionButton("back3", h4("zurück zu Schritt 3", style = "color: lightgrey"))
              ),
              column(6,
                actionButton("next5", h4("weiter zu Schritt 5", style = "color: lightblue"))
              )
            )
          ),
          column(8,
            uiOutput("text_numberOfEmptyTests")
          )
        )
      )
    ),
    tabPanel("Schritt 5: Rohwertverteilung",
      fluidPage(
        fluidRow(
          column(12,
            h1("Schritt 5: Rohwertverteilung", style = "color: lightblue"),
            p("Betrachten Sie die Verteilung der erreichten Punkte in der gewünschten Darstellung."),
            tags$hr()
          )
        ),
        fluidRow(
          column(4,
            selectInput("distributionPlotDecision", label = "Wählen Sie eine Darstellung.", choices = list("Histogramm", "Quantil-Quantil-Plot"), selected = "Histogramm"),
            conditionalPanel("input.distributionPlotDecision == 'Quantil-Quantil-Plot'",
              tags$hr(),
              fluidRow(
                column(1,
                  icon("info-circle", "fa-2x")
                ),
                column(11,
                  p("Ein", strong("Quantil-Quantil-Plot"), "kann zum Vergleich zweier Verteilungen herangezogen werden. Hier wird die Rohwertverteilung der erreichten Punkte mit einer Normalverteilung (rote Linie) verglichen.")
                )
              )
            ),
            tags$hr(),
            fluidRow(
              column(6,
                actionButton("back4", h4("zurück zu Schritt 4", style = "color: lightgrey"))
              ),
              column(6,
                actionButton("next6", h4("weiter zu Schritt 6", style = "color: lightblue"))
              )
            )
          ),
          column(8,
            plotOutput("plot_distribution")
          )
        )
      )
    ),
    tabPanel("Schritt 6: Itemkennwerte",
      fluidPage(
        fluidRow(
          column(12,
            h1("Schritt 6: Itemkennwerte", style = "color: lightblue"),
            p("Betrachten Sie die Itemschwierigkeiten und -trennschärfen."),
            p("Entscheiden Sie, welche Items für die Notengebung berücksichtigt werden sollen.", br(), "Dies kann wahlweise mit dem cutoff-Regler in der Seitenleiste links oder manuell durch Klicken auf die Checkboxen geschehen."),
            tags$hr()
          )
        ),
        fluidRow(
          column(4,
            uiOutput("itemCutoffInput"),
            tags$hr(),
            fluidRow(
              column(1,
                icon("info-circle", "fa-2x")
              ),
              column(11,
                p("Die", strong("Itemschwierigkeit"), "wird durch einen Index gekennzeichnet, der dem Anteil derjenigen Personen entspricht, die das Item richtig lösen oder bejahen.")
              )
            ),
            fluidRow(
              column(1,
                icon("info-circle", "fa-2x")
              ),
              column(11,
                p("Der", strong("Trennschärfe"), "eines Items ist zu entnehmen, wie gut das gesamte Testergebnis aufgrund der Beantwortung eines einzelnen Items vorhersagbar ist.")
              )
            ),
            p("Bortz, J., & Döring, N. (2006).", em("Forschungsmethoden und evaluation: für human-und sozialwissenschaftler."), "Springer."),
            tags$hr(),
            fluidRow(
              column(6,
                actionButton("back5", h4("zurück zu Schritt 5", style = "color: lightgrey"))
              ),
              column(6,
                actionButton("next7", h4("weiter zu Schritt 7", style = "color: lightblue"))
              )
            )
          ),
          column(8,
            uiOutput("itemStatsLayout")
          )
        )
      )
    )
  ),
  
  navbarMenu("Notengebung und Bericht",
    tabPanel("Schritt 7: Notengebung",
      fluidPage(
        fluidRow(
          column(12,
            h1("Schritt 7: Notengebung", style = "color: lightblue"),
            p("Legen Sie mithilfe der beiden Schiebergler in der Seitenleiste links die benötigten Mindestpunktzahlen für die Noten 4 und 6 fest."),
            p("Im Hauptfenster sehen Sie, wie sich Ihre Entscheidungen auf die Verteilung der Noten auswirken."),
            tags$hr()
          )
        ),
        fluidRow(
          column(4,
            fluidRow(
              column(4,
                uiOutput("text_passedInfo")
              ),
              column(4,
                uiOutput("text_failedInfo")
              ),
              column(4,
                uiOutput("text_meanInfo")
              )
            ),
            tags$hr(),
            fluidRow(
              column(1,
                icon("warning", "fa-2x")
              ),
              column(11,
                p("Diese Angaben beziehen sich auf", strong("nicht leere"), "Prüfungen!")
              )
            ),
            tags$hr(),
            uiOutput("slider4"),
            uiOutput("slider6"),
            tags$hr(),
            fluidRow(
              column(6,
                actionButton("back6", h4("zurück zu Schritt 6", style = "color: lightgrey"))
              ),
              column(6,
                actionButton("next8", h4("weiter zu Schritt 8", style = "color: lightblue"))
              )
            )
          ),
          column(8,
            fluidRow(
              column(4,
                plotOutput("curve_function")
              ),
              column(8,
                plotOutput("hist_grades")
              )
            )
          )
        )
      )
    ),
    tabPanel("Schritt 8: Bericht",
      fluidPage(
        fluidRow(
          column(12,
            h1("Schritt 8: Bericht", style = "color: lightblue"),
            p("Sie sind am Ende der Auswertung angekommen."),
            p("Mit einem Klick auf", em("Notentabelle"), "erhalten Sie eine Tabelle der Schlussbenotung (Vorschau im Hauptfenster), durch Klicken auf", em("Download"), "einen Bericht zu Ihrer Auswertung."),
            tags$hr()
          )
        ),
        fluidRow(
          column(4,
            downloadButton("csv", "Notentabelle (CSV)"),
            tags$br(),
            tags$br(),
            downloadButton("report", "Auswertungsbericht (PDF)"),
            tags$hr(),
            fluidRow(
              column(6,
                actionButton("back7", h4("zurück zu Schritt 7", style = "color: lightgrey"))
              ),
              column(6,
                actionButton("next0", h4("zum Anfang", style = "color: lightblue"))
              )
            )
          ),
          column(8,
            dataTableOutput("table_grades")
          )
        )
      )
    )
  )
  
))
