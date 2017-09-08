outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            h3("Input Summary"),
            tableOutput("summary"),
            modalButton('toggle me', icon = NULL)
        ),
    
        mainPanel(
            h3("Output"),
            plotOutput(outputId = "test_plot")
        )
    )
}