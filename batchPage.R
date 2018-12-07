batchPage <- function() {fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    sidebarLayout(
        sidebarPanel(
            width = 3,
            div(id="file_div", fileInput("kmer_files", "K-mer profiles", multiple = TRUE)),
            h3("Input Settings"),
            numericInput("batch_kmer_length", "K-mer length", 21),
            numericInput("batch_read_length", "Read length", 149),
            numericInput("batch_min_kmer", "Min K-mer Cutoff", 5),
            numericInput("batch_max_kmer", "Max K-mer Cutoff", 100),
           
            ###################################################################start
            ###GenomeScope Settings-A toggle switch to turn a selection on or off###
            ########################################################################
            
            materialSwitch(
                inputId = "gscope_batch_toggle", 
                label = tags$b("GenomeScope Settings"), 
                value = FALSE, 
                status = "primary"),
            hidden(
                fixedRow(
                    id = "gscope_batch_settings",
                    column(
                        width = 12,
                        fixedRow(
                            bsTooltip(id = "batch_gscope_num_rounds", title = "Cutoff for number of rounds of model fitting",
                                      placement = "right", trigger = "hover",
                                      options = list(container = "body")),
                            bsTooltip(id = "batch_gscope_start_shift", 
                                      title = "Everything below this point will always be considered error",
                                      placement = "right", trigger = "hover",
                                      options = list(container = "body")),
                            column(width = 6, numericInput("batch_gscope_num_rounds", "Number of rounds", value = 4, min = 1)),
                            column(width = 6, numericInput("batch_gscope_start_shift", "Start shift", value = 5, min = 0))
                        ),
                        fixedRow(
                            bsTooltip(id = "batch_gscope_error_cutoff", title = "Initial min kmer cutoff/end of genomescope error curve",
                                      placement = "right", trigger = "hover",
                                      options = list(container = "body")),
                            bsTooltip(id = "batch_gscope_max_iter", title = "Iteration cutoff for nls",
                                      placement = "right", trigger = "hover",
                                      options = list(container = "body")),
                            column(width = 6, numericInput("batch_gscope_error_cutoff", "Error cutoff", value = 15, min = 1)),
                            column(width = 6, numericInput("batch_gscope_max_iter", "Max iteration", value = 20, min = 1))
                        ),
                        fixedRow(
                            bsTooltip(id = "batch_gscope_score_close", 
                                      title = "Percentage improvement required to replace existing model each iteration",
                                      placement = "right", trigger = "hover",
                                      options = list(container = "body")),
                            bsTooltip(id = "batch_gscope_het_diff", 
                                      title = "Heterozygosity fold difference cutoff to overrule heterozygosity",
                                      placement = "right", trigger = "hover",
                                      options = list(container = "body")),
                            column(width = 6, numericInput("batch_gscope_score_close", "Score difference percentage", value = 0.2, 
                                                           min = 0, step = 0.1)),
                            column(width = 6, numericInput("batch_gscope_het_diff", "Heterozygosity fold difference", value = 10, min = 0))
                        )
                    )
                )
            ),
            #########################################################################end
            submitButton("Submit")
        ),
        
        mainPanel(
            width = 9,
            h3("Results"),
            hidden(
                fixedRow(
                    id = "batch_download",
                    column(
                        width = 12,
                        downloadButton("batch_size_csv", "Download size predictions as csv"),
                        downloadButton("batch_stats_csv", "Download GenomeScope statistics as csv")
                    )
                )
            ),
            hidden(
                h4("Size Predictions", id = "batch_size_header")
            ),
            withSpinner(tableOutput("batch_sizes_table")),
            hidden(
                fixedRow(
                    id = "batch_stats_elems",
                    column(
                        width = 12,
                        
                        h4("GenomeScope Statistics"),
                        withSpinner(tableOutput("batch_stats_table"))
                    )
                )
            )
        )
    )
)}
