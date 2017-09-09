outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            h3("Input Summary"),
            div(class="table-responsive", style="border: none;", tableOutput("summary")),
            sliderInput("start_freq", "Starting Frequency", min = 1, max = 100, value = 0)
        ),
    
        mainPanel(
            h3("Output Model"),
            h4("Count VS Frequency", align="center"),
            plotlyOutput("simple_plot"),
            h3("Estimated Size"),
            textOutput("simple_size")
        )
    )
}