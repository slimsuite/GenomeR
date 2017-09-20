outputPage <- function() {fixedPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        position = "right",
        sidebarPanel(
            h3("Model Settings"),
            radioGroupButtons(inputId = "show_hide_button", label = NULL,
                justified = TRUE, status = "default",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                 no = icon("remove", lib = "glyphicon")),
                choices = c("Show all" = "show_all", "Hide error" = "hide_error")
            ),
            
            radioGroupButtons(inputId = "genome_type", label = NULL,
                justified = TRUE, status = "default",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                 no = icon("remove", lib = "glyphicon")),
                choices = c("Haploid" = "genome_haploid", "Diploid" = "genome_diploid")
            ),

            br(),
            uiOutput("freq_slider"),
            
            h3("Simple Count Size"),
            textOutput("simple_size"),
            
            h3("Peak Frequency Size"),
            textOutput("freq_size")
        ),
        
        mainPanel(
            h3("Output Model"),
            tabsetPanel(type = "tabs", id="plot-tabs",
                tabPanel("Unique Kmer Count", 
                    h4("Count vs Frequency", align="center"),
                    plotlyOutput("simple_count_plot")
                ),
                tabPanel("Peak Frequency", 
                    h4("Count vs Frequency", align="center"),
                    plotlyOutput("peak_freq_plot")
                ),
                tabPanel("Genome Scope",
                    tabsetPanel(
                        tabPanel("Plot 1",
                            h4("Count vs Frequency", align="center"),
                            plotOutput("genome_scope_plot_1")
                        ),
                        tabPanel("Plot 2",
                            h4("Count vs Frequency", align="center"),
                            plotOutput("genome_scope_plot_2")
                        )
                    )
                )
            )
        )
    )
)}

