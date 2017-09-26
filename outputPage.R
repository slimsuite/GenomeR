outputPage <- function() {fixedPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        position = "left",
        sidebarPanel(
            h3("Model Settings"),
            checkboxGroupButtons(inputId = "show_hide_button", label = NULL,
<<<<<<< HEAD
                                 justified = FALSE, status = "default",
                                 checkIcon = list(yes = div(icon("eye-open", lib = "glyphicon"), "Showing error"),
                                                  no = div(icon("eye-close", lib = "glyphicon"), "Hiding    error")),
                                 choices = c(" " = "show_error")
=======
                justified = FALSE, status = "default",
                checkIcon = list(yes = div(icon("eye-open", lib = "glyphicon"), "Showing error"),
                                 no = div(icon("eye-close", lib = "glyphicon"), "Hiding    error")),
                choices = c(" " = "show_error")
>>>>>>> 64d70a06385dd05a12eb2aec632c33f1277765f0
            ),
            
            radioGroupButtons(inputId = "genome_type", label = NULL,
                              justified = FALSE, status = "default",
                              checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                               no = icon("remove", lib = "glyphicon")),
                              choices = c("Haploid" = "haploid", "Diploid" = "diploid")
            ),
            
            numericInput("kmer_length", "K-mer length", 21),
            numericInput("read_length", "Read length", 100),
            numericInput("max_kmer_coverage", "Maximum k-mer coverage", 100),
            
            br(),
            uiOutput("minkmer_slider"),
            uiOutput("maxkmer_slider"),
            
            #tableOutput("size_table"),
            
            h3("GenomeScope Size"),
            textOutput("gscope_size"),
            
            h3("Simple Count Size"),
            textOutput("simple_size"),
            
            h3("Peak Frequency Size"),
            textOutput("freq_size")
            
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
