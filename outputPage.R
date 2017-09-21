outputPage <- function() {fixedPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        position = "right",
        sidebarPanel(
            h3("Model Settings"),
            checkboxGroupButtons(inputId = "show_hide_button", label = NULL,
                justified = FALSE, status = "default",
                checkIcon = list(yes = icon("eye-close", lib = "glyphicon"),
                                 no = icon("eye-open", lib = "glyphicon")),
                choices = c(" " = "hide_error")
            ),
            
            radioGroupButtons(inputId = "genome_type", label = NULL,
                justified = FALSE, status = "default",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                 no = icon("remove", lib = "glyphicon")),
                choices = c("Haploid" = "haploid", "Diploid" = "diploid")
            ),

            br(),
            uiOutput("minkmer_slider"),
            uiOutput("maxkmer_slider"),
            
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
)}

