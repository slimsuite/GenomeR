outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            h3("Input Summary"),
            div(class="table-responsive", style="border: none;", tableOutput("summary")),
            
            h3("Model Settings"),
            radioGroupButtons(
                inputId = "show_hide_button", label = NULL, 
                choices = c("Show all", "Hide error"), 
                justified = FALSE, status = "default"
            ),
            uiOutput("freq_slider"),
            
            h3("Simple Count Size"),
            textOutput("simple_size"),
            
            h3("Peak Frequency Size"),
            textOutput("freq_size"),

            h3("GenomeScope Size"),
            textOutput("gscope_size")
        ),
        
        mainPanel(
            h3("Output Model"),
            selectInput(
                "plot_type",
                "Model to plot",
                c("GenomeScope" = "gscope", "Simple Count" = "simple", "Peak Frequency" = "peak"),
                "gscope"
            ),
            radioGroupButtons(
                "gscope_type",
                choices = c("Linear Plot" = "linear", "Log Plot" = "log"),
                selected = "linear",
                justified = TRUE
            ),
            h4("Count vs Frequency", align="center"),
            plotlyOutput("plot"),
            tableOutput("gscope_summary")
        )
    )
}

