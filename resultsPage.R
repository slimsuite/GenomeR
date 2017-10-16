library(shinyBS)
resultsPage <- function() {fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        sidebarPanel(
            h3("Input Summary"),
            div(class="table-responsive", style="border: none;", tableOutput("input_summary")),
            h3("Report Download"),
            radioGroupButtons("report_format", "Select format", c("HTML", "PDF", "Word")),
            downloadButton("report", "Generate report")
        ),
        
        mainPanel(
            h3("Report"),
            bsButton("render_cutoff_plot", "Generate Report", size="large", type="toggle"),
            # textOutput("summary"),
            conditionalPanel('input.render_cutoff_plot',
                h4("Size vs Maximum Kmer Cutoff"),
                plotlyOutput("cutoff_plot"),
                tableOutput("cutoff_table")
            )
        )
    )
)}