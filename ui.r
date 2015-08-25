shinyUI(fluidPage(
  
  tags$style(type='text/css', '
    body { padding: 5vh }
    .tab-content { height: 90vh; overflow-y: auto; padding-left: 1vw; }
  '),
  
  navlistPanel('inf4oec', widths = c(2, 10),
    
    tabPanel('Step 1: Data Upload',
      h1('Data Upload'),
      p('Upload the data you want to analyse. It needs to have the following columns: "Matrikelnummer", "Serie", "Fragenummer", alternatives ("A", "B", "C" etc.) and "Punkte".'),
      uiOutput("confirmation"),
      tags$hr(),
      fileInput('inputFile', '', accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv')),
      checkboxInput('header', 'Header', TRUE),
      tags$br(),
      radioButtons('sep', tags$b('Separator'), c(Comma = ',', Semicolon = ';', Tab = '\t'), ','),
      tags$hr()
    ),
    
    tabPanel('Step 2: Validation',
      h1('Validation'),
      p('Make sure all the data below is correct.'),
      htmlOutput('validationTable')
    ),
    
    tabPanel('Step 3: Item Statistics',
      h1('Item Statistics')
    )
    
  )
  
))