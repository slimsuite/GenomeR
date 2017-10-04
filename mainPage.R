mainPage <- function() {fixedPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        position = "left",
        sidebarPanel(
            
            # model settings
            h3("Model Settings"),
            checkboxGroupButtons(inputId = "show_hide_button", label = NULL,

                justified = FALSE, status = "default",
                checkIcon = list(yes = div(icon("eye-open", lib = "glyphicon"), "Showing error"),
                                 no = div(icon("eye-close", lib = "glyphicon"), "Hiding    error")),
                choices = c(" " = "show_error")

            ),
            radioGroupButtons(inputId = "genome_type", label = NULL,
                              justified = FALSE, status = "default",
                              checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                               no = icon("remove", lib = "glyphicon")),
                              choices = c("Haploid" = "haploid", "Diploid" = "diploid")
            ),
            uiOutput("minkmer_slider"),
            uiOutput("maxkmer_slider"),
            
            # output size summary
            tableOutput("size_table"),
            
            # inputs
            h3("Input Settings"),
            selectInput("type", label = NULL,
                choices = c("File upload" = "file", "Select sample" = "sample", "Simulation data" = "simulation")
            ),
            # radioGroupButtons(
            #     inputId = "type", label = NULL, 
            #     choices = c("File input", "Simulation input"), 
            #     justified = TRUE, status = "primary",
            #     checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            # ),
            
            # file input
            div(id = "input-col",
                h4("File input"),
                fileInput("kmer_file", "K-mer profile"),
                numericInput("kmer_length", "K-mer length", 21),
                numericInput("read_length", "Read length", 149),
                numericInput("max_kmer_coverage", "Maximum k-mer coverage", 100)
            ),
            
            # select sample
            div(id = "sample-col",
                h4("Sample selection"),
                selectInput("sample", "Choose a sample k-mer profile",
                            c("simulation" = "Select sample", "small" = "www/small.histo", 
                              "sharky" = "www/sharky.histo")
                )
            ),
            
            # simulation settings
            div(id = "sim-col",
                h4("Simulation generation"),
                numericInput("sim_genome_size", "Genome size", 3000000000),
                radioGroupButtons(inputId = "sim_genome_type", label = "Ploidy type",
                                  choices = c("Haploid" = "sim_haploid", "Diploid" = "sim_diploid")
                ),
                numericInput("sim_heterozygosity", "Heterozygosity (%)", 25)    
            )
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
