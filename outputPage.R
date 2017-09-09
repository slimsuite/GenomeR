outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            h3("Input Summary"),
            div(class="table-responsive", style="border: none;", tableOutput("summary")),
            uiOutput("start_freq_slider"),
            uiOutput("end_freq_slider")
        ),
    
        mainPanel(
            h3("Output Model"),
            h4("Count vs Frequency", align="center"),
            plotlyOutput("simple_plot"),
            h3("Estimated Size"),
            textOutput("simple_size")
        )
    )
}
