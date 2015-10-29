shinyUI(fluidPage(
  
  tags$style(type='text/css', '
    body { padding: 5vh }
    .tab-content { height: 90vh; overflow-y: auto; padding-left: 1vw; }
  '),
  
  navlistPanel('inf4oec', widths = c(2, 10),
    
    tabPanel('Upload',
      h1('Upload'),
      p('Upload the data you want to analyse. It needs to have the following columns: "Matrikelnummer", "Serie", "Fragennummer", "A", "B", "C", "D", "E" and "Punkte".'),
      tags$hr(),
      fileInput('inputFile', '', accept = 'text/csv'),
      uiOutput("confirmation")
    ),
    
    tabPanel('Validation',
      h1('Validation'),
      p('Make sure all the data below is correct.'),
      tags$hr(),
      htmlOutput('validationTable')
    ),
    
    tabPanel('Step 3: Item Statistics',
      h1('Item Statistics'),
      p('These statistics are meant to help you ...'),
      tags$hr(),
      textOutput("weg"),
      htmlOutput('statisticsTable')
    ),
    
    tabPanel('Step 5: Points received',
    h1('fff'),
             tags$hr(),
             dataTableOutput("oh2")
    )
    
  )
  
))