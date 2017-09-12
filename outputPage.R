library(plotly)
library(shiny)

outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            h3("Input Summary"),
            div(class="table-responsive", style="border: none;", tableOutput("summary")),
            
            h3("Model Settings"),
            uiOutput("freq_slider"),
            
            h3("Simple Count Size"),
            textOutput("simple_size"),
            
            h3("Peak Frequency Size"),
            textOutput("freq_size")
        ),
        
        mainPanel(
            h3("Output Model"),
            tabsetPanel(type = "tabs", id="plot-tabs",
                tabPanel("Simple Count", 
                    h4("Count vs Frequency", align="center"),
                    plotlyOutput("simple_count_plot")
                ),
                tabPanel("Method 2", 
                    h4("Count vs Frequency", align="center"),
                    plotlyOutput("peak_freq_plot")
                ),
                tabPanel("Genome Scope",
                    h4("Count vs Frequency", align="center"),
                    plotOutput("genome_scope_plot")
                )
            )
        )
    )
}

