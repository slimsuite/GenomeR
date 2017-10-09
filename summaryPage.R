summaryPage <- function() {fixedPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        sidebarPanel(
            h3("Input Summary"),
            div(class="table-responsive", style="border: none;", tableOutput("input_summary"))
        ),
        
        mainPanel(
            h3("Plot"),
            div(plotlyOutput("cutoff_plot")),
            h3("Report"),
            textOutput("summary"),
            radioGroupButtons("report_format", "Select format", c("HTML", "PDF", "Word")),
            downloadButton("report", "Generate report")
        )
    )
)}