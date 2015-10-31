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
      uiOutput("confirmation"),
      textOutput("test")
    ),
    
    tabPanel('Validation',
      h1('Validation'),
      p('Make sure all the data below is correct.'),
      tags$hr(),
      htmlOutput('validationTable')
    ),
    
    tabPanel('Distribution',
      h1('Distribution'),
      p('Check the distribution of the raw scores.'),
      tags$hr()
    ),
    
    tabPanel('Items',
      h1('Items'),
      p('Select the items that should be considered for the grading.'),
      tags$hr()
    ),
    
    tabPanel('Grades',
      h1('Grades'),
      p('Define the number of points needed for grades 4 and 6.'),
      tags$hr
    ),
    
    tabPanel('Report',
      h1('Report'),
      p(''),
      tags$hr
    )
    
  )
  
))