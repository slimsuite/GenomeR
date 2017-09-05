outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            "Output Summary",
            tableOutput("summary")
        ),
    
        mainPanel(
            h1("Output"),
            plotOutput(outputId = "test_plot")
        )
    )
}