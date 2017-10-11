mainPage <- function() {fixedPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        position = "left",
        sidebarPanel(
                        # inputs
            h3("Input Settings"),
            # selectInput("type", label = NULL,
            #     choices = c("File upload" = "file", "Select sample" = "sample", "Simulation data" = "simulation")
            # ),
            radioButtons(
                inputId = "type", label = NULL,
                choices = c("File upload" = "file", "Select sample" = "sample", "Simulation data" = "simulation")
                # justified = TRUE, status = "primary",
                # checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            ),

            # file input
            conditionalPanel('input.type === "file"',
                h4("File input"),
                fileInput("kmer_file", "K-mer profile")
            ),

            # select sample
            conditionalPanel('input.type === "sample"',
                h4("Sample selection"),
                selectInput("sample", "Choose a sample k-mer profile",
                            c("Select sample" = "",
                              "Small" = "www/small.histo",
                              "Sharky" = "www/sharky.histo",
                              "Pear" = "www/pear.histo",
                              "Seabass" = "www/seabass.histo")
                )
            ),

            # simulation settings
            conditionalPanel('input.type === "simulation"',
                h4("Simulation generation"),
                numericInput("sim_genome_size", "Genome size", 3000000000),
                radioGroupButtons(inputId = "sim_genome_type", label = "Ploidy type",
                                  choices = c("Haploid" = "sim_haploid", "Diploid" = "sim_diploid")
                ),
                numericInput("sim_heterozygosity", "Heterozygosity (%)", 25)
            ),
            
            # output size summary
            tableOutput("size_table"),
            
            # model settings
            h3("Model Settings"),
            conditionalPanel('input.plot_type === "simple" || input.plot_type === "peak"',
                checkboxGroupButtons(inputId = "show_hide_button", label = NULL,
                    justified = FALSE, status = "default",
                    checkIcon = list(yes = div(icon("eye-open", lib = "glyphicon"), "Showing error"),
                        no = div(icon("eye-close", lib = "glyphicon"), "Hiding    error")),
                    choices = c(" " = "show_error")        
                )
            ),
            conditionalPanel('input.plot_type === "peak"',
                radioGroupButtons(inputId = "genome_type", label = NULL,
                              justified = FALSE, status = "default",
                              checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                               no = icon("remove", lib = "glyphicon")),
                              choices = c("Haploid" = "haploid", "Diploid" = "diploid")
                )
            ),
            conditionalPanel(
                'input.plot_type === "simple" || input.plot_type === "peak"',
                hidden(
                    fixedRow(
                        id = "min_kmer_slider_cont",
                        column(
                            width = 12,
                            uiOutput("minkmer_slider")
                        )
                    )
                )
            ),
            hidden(
                fixedRow(
                    id = "max_kmer_slider_cont",
                    column(
                        width = 12,
                        uiOutput("maxkmer_slider")
                    )
                )
            ),
            conditionalPanel(
                "input.plot_type === 'gscope'",
                numericInput("kmer_length", "K-mer length", 21),
                numericInput("read_length", "Read length", 149),
                materialSwitch(
                    inputId = "gscope_adv_toggle", 
                    label = tags$b("Advanced Settings"), 
                    value = FALSE, 
                    status = "primary"),
                hidden(
                    fixedRow(
                        id = "gscope_adv_settings",
                        column(
                            width = 12,
                            fixedRow(
                                column(width = 6, numericInput("gscope_num_rounds", "Number of rounds", value = 4, min = 1)),
                                column(width = 6, numericInput("gscope_start_shift", "Start shift", value = 5, min = 0))
                            ),
                            fixedRow(
                                column(width = 6, numericInput("gscope_error_cutoff", "Error cutoff", value = 15, min = 1)),
                                column(width = 6, numericInput("gscope_max_iter", "Maximum iteration", value = 20, min = 1))
                            ),
                            fixedRow(
                                column(width = 6, numericInput("gscope_score_close", "Score difference percentage", value = 0.2, min = 0, 
                                                               step = 0.1)),
                                column(width = 6, numericInput("gscope_het_diff", "Heterozygosity fold difference", value = 10, min = 0))
                            )
                        )
                    )
                )
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
            hidden(
                fixedRow(
                    id = "output_elems",
                    column(
                        width = 12,
                        conditionalPanel(
                            'input.plot_type === "gscope"',
                            radioGroupButtons(
                                "gscope_type",
                                choices = c("Linear Plot" = "linear", "Log Plot" = "log"),
                                selected = "linear",
                                justified = TRUE
                            )
                        ),
                        h4("Count vs Frequency", align="center"),
                        plotlyOutput("plot"),
                        tableOutput("gscope_summary")
                    )
                )
            )
        )
    )
)}
