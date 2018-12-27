batchPage <- function() {fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    
    tabsetPanel(
        tabPanel(title = "Notes",
                 h4("Notes: Here are some examples"),
                 #
                 p("1. If the name of files includes the information of k-mer, read length and max cutoff, 
                   this information will be extracted from the name of uploading files."),
                 strong(" - Upload"),
                 p(),
                 code("brownsnake.10x.HiX.k21.r125.6.10k.histo"),
                 p(),
                 strong(" - Return"),
                 p(),
                 img(src = "nameinfo.png", height = 90, width = 800),
                 #
                 br(),
                 p("2. If the name of files does not include the information of k-mer, read length and max cutoff, 
                   this tool will return a table with default values and caculate by these values. K-mer
                   (k21); Read length (r149.0); Max cutoff (10k =10,000) "),
                 strong(" - Upload"),
                 p(),
                 code("TS_Child1_S3_L001.mer_counts.histo"),
                 p(),
                 strong(" - Return"),
                 p(),
                 img(src = "namewithoutinfo.png", height = 90, width = 800),
                 #
                 br(),
                 p("3. If users want to change these default values, they can download it as csv, 
                   change values, upload it and caculate it with this new table.  "),
                 strong(" - Download"),
                 p(),
                 img(src = "DownloadSummary.png", height = 40, width = 200),
                 p(),
                 strong(" - Upload"),
                 p(),
                 img(src = "chooseCsv.png", height = 60, width = 200)
                 ),
        
        
        tabPanel(title = "K-mer profiles",
    
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    div(id="file_div", fileInput("kmer_files", "Choose K-mer profiles", multiple = TRUE,accept = '.histo')),
                    
                    br(),
                    h3("Input Settings"),
                    numericInput("batch_min_kmer", "Min K-mer Cutoff", 5),
                   
                    ###################################################################start
                    ###GenomeScope Settings-A toggle switch to turn a selection on or off###
                    ########################################################################
                    
                    h4("GenomeScope Settings (Optional)"),
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
                        
                    ),
                    #########################################################################end
                    submitButton("Submit")
                ),
                
                mainPanel(
                    tabsetPanel(
                        tabPanel(title = "Summary",
                                 h3("Results", id = "batch_files"),
                                 fixedRow(
                                     id = "batch_download",
                                     column(
                                         width = 12,
                                         downloadButton("batch_summary_csv", "Download summary as csv")
                                     )
                                 ),
                                 h4("Summary"),
                                 dataTableOutput("batch_files_table")
                                 
                        ),
                        tabPanel(title = "Size predictions",
                                 width = 9,
                                 h3("Results"),
                                 fixedRow(
                                     id = "batch_download",
                                     column(
                                         width = 12,
                                         downloadButton("batch_size_csv", "Download size predictions as csv")
                                     )
                                 ),
                                 h4("Size Predictions", id = "batch_size_header"),
                                 withSpinner(dataTableOutput("batch_sizes_table"))
                                 #withSpinner(dataTableOutput("csv_sizes_table"))
                        ),
                        tabPanel(title = "GenomeScope statistic",
                                 width = 9,
                                 h3("Results"),
                                 fixedRow(
                                     id = "batch_download",
                                     column(
                                         width = 12,
                                         downloadButton("batch_stats_csv", "Download GenomeScope statistics as csv")
                                     )
                                ),
                                h4("GenomeScope Statistics", id = "batch_stats_elems"),
                                withSpinner(dataTableOutput("batch_stats_table"))
                                #withSpinner(dataTableOutput("csv_stats_table"))
                            )

                        )
                    )
    )),
    tabPanel(title = "K-mer profiles + csv file",
             
             sidebarLayout(
                 sidebarPanel(
                     width = 3,
                     div(id = "csv_div_1",fileInput("csv_file","Choose csv file",multiple = F,accept = ".csv")),
                     div(id="file_div_1", fileInput("kmer_files_1", "Choose K-mer profiles", multiple = TRUE,accept = '.histo')),
                     br(),
                     h3("Input Settings"),
                     numericInput("batch_min_kmer_1", "Min K-mer Cutoff", 5),
                     
                     ###################################################################start
                     ###GenomeScope Settings-A toggle switch to turn a selection on or off###
                     ########################################################################
                     
                     h4("GenomeScope Settings (Optional)"),
                     fixedRow(
                         id = "gscope_batch_settings",
                         column(
                             width = 12,
                             fixedRow(
                                 bsTooltip(id = "batch_gscope_num_rounds_1", title = "Cutoff for number of rounds of model fitting",
                                           placement = "right", trigger = "hover",
                                           options = list(container = "body")),
                                 bsTooltip(id = "batch_gscope_start_shift_1", 
                                           title = "Everything below this point will always be considered error",
                                           placement = "right", trigger = "hover",
                                           options = list(container = "body")),
                                 column(width = 6, numericInput("batch_gscope_num_rounds_1", "Number of rounds", value = 4, min = 1)),
                                 column(width = 6, numericInput("batch_gscope_start_shift_1", "Start shift", value = 5, min = 0))
                             ),
                             fixedRow(
                                 bsTooltip(id = "batch_gscope_error_cutoff_1", title = "Initial min kmer cutoff/end of genomescope error curve",
                                           placement = "right", trigger = "hover",
                                           options = list(container = "body")),
                                 bsTooltip(id = "batch_gscope_max_iter_1", title = "Iteration cutoff for nls",
                                           placement = "right", trigger = "hover",
                                           options = list(container = "body")),
                                 column(width = 6, numericInput("batch_gscope_error_cutoff_1", "Error cutoff", value = 15, min = 1)),
                                 column(width = 6, numericInput("batch_gscope_max_iter_1", "Max iteration", value = 20, min = 1))
                             ),
                             fixedRow(
                                 bsTooltip(id = "batch_gscope_score_close_1", 
                                           title = "Percentage improvement required to replace existing model each iteration",
                                           placement = "right", trigger = "hover",
                                           options = list(container = "body")),
                                 bsTooltip(id = "batch_gscope_het_diff_1", 
                                           title = "Heterozygosity fold difference cutoff to overrule heterozygosity",
                                           placement = "right", trigger = "hover",
                                           options = list(container = "body")),
                                 column(width = 6, numericInput("batch_gscope_score_close_1", "Score difference percentage", value = 0.2, 
                                                                min = 0, step = 0.1)),
                                 column(width = 6, numericInput("batch_gscope_het_diff_1", "Heterozygosity fold difference", value = 10, min = 0))
                             )
                         )
                         
                     ),
                     #########################################################################end
                     submitButton("Submit")
                 ),
                 
                 mainPanel(
                     tabsetPanel(
                         tabPanel(title = "Summary",
                                  id = "batch_files_1",
                                  h5("Please upload one or more jellyfish kmer profile(s)  AND  a summary csv file.")
            ,
                                  dataTableOutput("new_csv")
                                  
                         ),
                         tabPanel(title = "Size predictions",
                                  width = 9,
                                  h3("Results"),
                                  fixedRow(
                                      id = "batch_download_1",
                                      column(
                                          width = 12,
                                          downloadButton("batch_size_csv_1", "Download size predictions as csv")
                                      )
                                  ),
                                  h4("Size Predictions", id = "batch_size_header_1"),
                                  withSpinner(dataTableOutput("batch_sizes_table_1"))
                                  #withSpinner(dataTableOutput("csv_sizes_table"))
                         ),
                         tabPanel(title = "GenomeScope statistic",
                                  width = 9,
                                  h3("Results"),
                                  fixedRow(
                                      id = "batch_download_1",
                                      column(
                                          width = 12,
                                          downloadButton("batch_stats_csv_1", "Download GenomeScope statistics as csv")
                                      )
                                  ),
                                  h4("GenomeScope Statistics", id = "batch_stats_elems_1"),
                                  withSpinner(dataTableOutput("batch_stats_table_1"))
                                  #withSpinner(dataTableOutput("csv_stats_table"))
                         )
                     )
                 )
             ))
    )
    
    
    
)}
