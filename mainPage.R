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
            bsTooltip(id = "type", title = "Select type of input",
                      placement = "right", trigger = "hover",
                      options = list(container = "body")),
            radioButtons(
                inputId = "type", label = NULL,
                choices = c("File upload" = "file", "Select sample" = "sample", "Simulation data" = "simulation")
                # justified = TRUE, status = "primary",
                # checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            ),

            # file input
            conditionalPanel('input.type === "file"',
                h4("File input"),
                bsTooltip(id = "file_div", title = "Upload a jellyfish kmer frequency profile",
                          placement = "right", trigger = "hover",
                          options = list(container = "body")),
                div(id="file_div", fileInput("kmer_file", "K-mer profile"))
            ),

            # select sample
            conditionalPanel('input.type === "sample"',
                h4("Sample selection"),
                bsTooltip(id = "sample", title = "Choose a jellyfish sample file to run",
                          placement = "right", trigger = "hover",
                          options = list(container = "body")),
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
                
                # inputs
                splitLayout(
                    bsTooltip(id = "sim_genome_size", title = "Number of bp of simulation genome",
                              placement = "right", trigger = "hover",
                              options = list(container = "body")),
                    bsTooltip(id = "sim_coverage", title = "Number of bp of simulation genome",
                              placement = "right", trigger = "hover",
                              options = list(container = "body")),
                    numericInput("sim_genome_size", "Genome size", 3000000, min=1000, step=1000000),
                    numericInput("sim_coverage", "Sequencing coverage", 50, step=10)
                ),
                splitLayout(
                    bsTooltip(id = "sim_max_kmer", title = "Limit kmer after which simulation will produce count of 0",
                              placement = "right", trigger = "hover",
                              options = list(container = "body")),
                    bsTooltip(id = "sim_error_rate", title = "Percentage of 'error' kmers produced for each unique kmer",
                              placement = "right", trigger = "hover",
                              options = list(container = "body")),
                    numericInput("sim_max_kmer", "Data cutoff", 300, min=100, step=50),
                    numericInput("sim_error_rate", "Error rate (%)", 5, step=5)
                ),
                radioGroupButtons(inputId = "sim_genome_type", label = "Ploidy type",
                                  choices = c("Haploid" = "sim_haploid", "Diploid" = "sim_diploid")
                ),
                bsTooltip(id = "sim_heterozygosity", title = "Percentage heterozygosity for diploid genome",
                          placement = "right", trigger = "hover",
                          options = list(container = "body")),
                numericInput("sim_heterozygosity", "Heterozygosity (%)", 25)
            ),
            
            # output size summary
            tableOutput("size_table"),
            
            # model settings
            h3("Model Settings"),
            conditionalPanel('input.plot_type === "simple" || input.plot_type === "peak"',
                bsTooltip(id = "show_hide_button", title = "Show full plot with error highlited in red, or only plot non-error kmer frequencies",
                          placement = "right", trigger = "hover",
                          options = list(container = "body")),
                checkboxGroupButtons(inputId = "show_hide_button", label = NULL,
                    justified = FALSE, status = "default",
                    checkIcon = list(yes = div(icon("eye-open", lib = "glyphicon"), "Showing error"),
                        no = div(icon("eye-close", lib = "glyphicon"), "Hiding    error")),
                    choices = c(" " = "show_error")        
                )
            ),
            conditionalPanel('input.plot_type === "peak"',
                bsTooltip(id = "genome_type", title = "Set Peak Frequency model for haploid/diploid prediction",
                          placement = "right", trigger = "hover",
                          options = list(container = "body")),
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
                            bsTooltip(id = "minkmer_slider", title = "Min kmer cutoff (to remove errors)",
                                      placement = "right", trigger = "hover",
                                      options = list(container = "body")),
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
                        bsTooltip(id = "maxkmer_slider", title = "Max kmer cutoff (to exclude high freq from prediction)",
                                  placement = "right", trigger = "hover",
                                  options = list(container = "body")),
                        uiOutput("maxkmer_slider")
                    )
                )
            ),
            conditionalPanel(
                "input.plot_type === 'gscope'",
                
                # tooltips
                bsTooltip(id = "kmer_length", title = "Kmer length as set when using jellyfish",
                          placement = "right", trigger = "hover",
                          options = list(container = "body")),
                bsTooltip(id = "read_length", title = "Read length as set when using jellyfish",
                          placement = "right", trigger = "hover",
                          options = list(container = "body")),

                # inputs
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
                                bsTooltip(id = "gscope_num_rounds", title = "Cutoff for number of rounds of model fitting",
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body")),
                                bsTooltip(id = "gscope_start_shift", title = "Initial min kmer cutoff",
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body")),
                                column(width = 6, numericInput("gscope_num_rounds", "Number of rounds", value = 4, min = 1)),
                                column(width = 6, numericInput("gscope_start_shift", "Start shift", value = 5, min = 0))
                            ),
                            fixedRow(
                                bsTooltip(id = "gscope_error_cutoff", title = "gscope_error_cutoff",
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body")),
                                bsTooltip(id = "gscope_max_iter", title = "gscope_max_iter",
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body")),
                                column(width = 6, numericInput("gscope_error_cutoff", "Error cutoff", value = 15, min = 1)),
                                column(width = 6, numericInput("gscope_max_iter", "Maximum iteration", value = 20, min = 1))
                            ),
                            fixedRow(
                                bsTooltip(id = "gscope_score_close", title = "gscope_score_close",
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body")),
                                bsTooltip(id = "gscope_het_diff", title = "gscope_het_diff",
                                          placement = "right", trigger = "hover",
                                          options = list(container = "body")),
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
                c("GenomeScope" = "gscope", "Peak Frequency" = "peak", "Simple Count" = "simple"),
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
                        withSpinner(plotlyOutput("plot")),
                        conditionalPanel(
                            'input.plot_type === "gscope"',
                            tableOutput("gscope_summary")
                        )
                    )
                )
            )
        )
    )
)}