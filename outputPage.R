outputPage <- function() {fixedPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        position = "left",
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
            
            numericInput("kmer_length", "K-mer length", 21),
            numericInput("read_length", "Read length", 100),
            numericInput("max_kmer_coverage", "Maximum k-mer coverage", 100),
            
            br(),
            uiOutput("freq_slider"),
            #print (output$simple_size),
            
            #model_table <- matrix(c("simple_size","freq_size", "genome_scope_size"), 
            #                        ncol = 3),
            #colnames(model_table) <- c("Simple Count Size", "Peak Frequency Size", 
             #                          "GenomeScope Size"),
            
            #model_table <- as.table(model_table),
            
            #renderTable(model_table),
    
            tableOutput("size_table"),
            
            h3("Simple Count Size"),
            textOutput("simple_size"),
            
            h3("Peak Frequency Size"),
            textOutput("freq_size"),

            h3("GenomeScope Size"),
            textOutput("genome_scope_size")
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
                    tabsetPanel(type = "tabs", id="plot-tabs",
                        tabPanel("Genome Scope Linear Plot",
                            h4("Count vs Frequency", align="center"),
                            plotlyOutput("genome_scope_linear_plot")
                        ),
                        tabPanel("Genome Scope Log Plot",
                            h4("Count vs Frequency", align="center"),
                            plotlyOutput("genome_scope_log_plot")
                        )
                    ),
                    h4("GenomeScope Results"),
                    tableOutput("genome_scope_summary")
                )
            )
        )
    )
)}

