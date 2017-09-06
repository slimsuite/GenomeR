outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            "Output Summary",
            tableOutput("summary")
        ),
    
        mainPanel(
            h1("Output"),
            tabsetPanel( 
                tabPanel("Counting", plotOutput(outputId = "test_plot")),
                tabPanel("Frequencies", plotOutput(outputId = "test_plot2"))
            )
        )
    )
}