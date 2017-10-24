library(shinyBS)
resultsPage <- function() {fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        sidebarPanel(width = 3,
            h3("Input Summary"),
            div(class="table-responsive", style="border: none;", tableOutput("input_summary")),
            h3("Report Download"),
            radioGroupButtons("report_format", "Select format", c("HTML", "PDF", "Word")),
            downloadButton("report", "Download report")
        ),
        
        mainPanel(width = 9,
            h3("Report"),
            bsButton("render_cutoff_plot", "Generate Report", size="large", type="toggle"),
            conditionalPanel('input.render_cutoff_plot',
                column(width = 8,
                    h4("Size vs Maximum Kmer Cutoff", align="center"),
                    withSpinner(plotlyOutput("cutoff_plot"))
                ),
                column(width = 4,
                    div(class='table-responsive', withSpinner(tableOutput("cutoff_table")))
                )
            )
        )
    )
)}