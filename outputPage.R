library(plotly)

outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            h3("Input Summary"),
            div(class="table-responsive", style="border: none;", tableOutput("summary")),
            
            h3("Model Settings"),
            uiOutput("freq_slider"),
            
            h3("Estimated Size"),
            textOutput("simple_size")
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
                tabPanel("Method 3", 
                    h4("Count vs Frequency", align="center")
                )
            )
        )
    )
}

