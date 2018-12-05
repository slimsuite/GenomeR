History <- function() {fluidPage(
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
             ),
    titlePanel("Review csv files"),
    p("Users could upload the csv results that they got from Batch analysis page and check the results again."),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            #Selector for file upload
            fileInput('datafile1', 'Choose CSV file: size prediction',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain')),
            fileInput('datafile2', 'Choose CSV file: GenomeScope statistics',
                      accept=c('text/csv', 'text/comma-separated-values,text/plain'))
        ),
        mainPanel(
            h4('Size Prediction'),
            dataTableOutput("filetable1"),
            br(),
            h4('GenomeScope Statistics'),
            dataTableOutput("filetable2")
        )
    )
 
)}













