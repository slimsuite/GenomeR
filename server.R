source("Install_package.R")     # install all packages needed in this app
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(tools)
library(knitr)
library(tidyverse)

source("simpleCountKmer.R")     # functions to estimate genome size
source("peakCountKmer.R")
source("genomeScope.R")
source("serverHelpers.R")       # helper functions used in server.R
source("simulation.R")          # simulates frequency/count data


shinyServer(function(input, output, session) {
    
    # hide useless tab
    shinyjs::hide(selector = "#navbar li a[data-value=nav_help]")
    
    #
    # Setup variables and any intermediary/conductor function
    #
    input_widgets = c("kmer_file", "kmer_length", "read_length")
    all_sim_widgets = c("sim_genome_size", "sim_genome_type", "sim_heterozygosity")
    sample_widgets = c("sample")
    init_elems = c("output_elems", "max_kmer_slider_cont", "min_kmer_slider_cont")
    
    #
    # Initial conditions
    #

    output$input_summary <- get_output_summary(input, input_widgets)
    
    observeEvent(input$gscope_adv_toggle, {
        if (input$gscope_adv_toggle == TRUE)
            shinyjs::show("gscope_adv_settings", anim = TRUE)
        else
            shinyjs::hide("gscope_adv_settings", anim = TRUE)
    })
    
    
    #
    # Object/Event listeners
    #
    
    # listener to enable heterozygosity only for diploid genomes
    observeEvent(input$sim_genome_type, {
        if (input$sim_genome_type == "sim_diploid")
            shinyjs::enable("sim_diploid_settings")
        else
            shinyjs::disable("sim_diploid_settings")
    })
    
    observeEvent(input$kmer_length, {
        if (input$kmer_length > input$read_length) {
            showNotification("Kmer-length cannot be greater than read length", type="error")
        }
    })
    
    observeEvent(input$read_length, {
        updateNumericInput(session, "kmer_length", max = input$read_length)
    })
    
    observeEvent(input$type, {
        if ((input$type == "file" && (is.null(input$kmer_file) || input$kmer_length > input$read_length)) ||
            (input$type == "sample" && !file.exists(input$sample)))
            toggle_settings(hide = init_elems, anim = TRUE, anim_type = "fade")
    })
    
    #
    # Reactive values
    #
    
    filename <- reactive({
        path = NULL
        name = NULL
        if (input$type == "file") {
            # check we actually have a file
            validate(
                need(input$kmer_file, "Please upload a jellyfish kmer profile"),
                need(input$kmer_length <= input$read_length, "Kmer-length cannot be greater than read length")
            )
            path = input$kmer_file$datapath
            name = file_path_sans_ext(basename(path))
        } else if (input$type == "sample") {
            validate(
                need(file.exists(input$sample), "Sample doesn't exist")
            )
            path = input$sample
            name = file_path_sans_ext(basename(input$sample))
        }
        return(list("name" = name, "path" = path))
    })
    
    # open file and save into data frame
    reactive_df <- reactive({
        if (input$type != "simulation") {
            file <- filename()$path
            # validate the data frame
            validate(
                need(try(df <- read.table(file)), paste("Could not read file: ", input$kmer_file$name)),
                need(ncol(df) == 2, "File does not have 2 columns")
            )
        } else {
            validate(
                need(input$sim_genome_size, "Please enter genome size"),
                need(input$sim_genome_type, "Please enter genome type"),
                need(input$sim_heterozygosity, "Please enter heterozygosity"),
                need(input$sim_coverage, "Please enter sequencing coverage"),
                need(input$sim_max_kmer, "Please enter a max kmer cutoff for generating sim data"),
                need(input$sim_error_rate, "Please enter an error rate for the simulation")
            )
            diploid = if (input$sim_genome_type == "sim_diploid") TRUE else FALSE
            df <- simulate(input$sim_genome_size, input$sim_coverage, input$sim_max_kmer, input$sim_error_rate, diploid, 
                           input$sim_kmer_length, input$sim_read_length, input$sim_heterozygosity / 100)
        }
        
        toggle_settings(show = init_elems, anim = TRUE, anim_type = "fade")
        names(df) = c("Frequency", "Count")
        rownames(df) = df$Frequency
        return(df)
    })
    
    # generate plots and size estimates
    simple_plot_data <- reactive({
        df <- reactive_df()
        if (is.null(input$simple_min_kmer) || is.null(input$simple_max_kmer)) {
            r = simple_count_kmer(df, show_error=FALSE)
        } else {
            if (is.null(input$show_hide_button) || input$show_hide_button == "hide_error") {
                show = FALSE
            } else if (input$show_hide_button == "show_error") {
                show = TRUE
            }
            
            r = simple_count_kmer(df, input$simple_min_kmer, input$simple_max_kmer, show_error=show)
        }
        return(r)
    })
    
    peak_plot_data <- reactive({
        df <- reactive_df()
        if (is.null(input$show_hide_button) || input$show_hide_button == "hide_error") {
            show = FALSE
        } else if (input$show_hide_button == "show_error") {
            show = TRUE
        }
        
        # auto chooses best peak_freq prediction
        if (input$genome_type == "auto") {
            haploid = peak_count_kmer(df, input$peak_min_kmer, input$peak_max_kmer, show_error = show, num_peaks = 1)
            diploid = peak_count_kmer(df, input$peak_min_kmer, input$peak_max_kmer, show_error = show, num_peaks = 2)
            simple_size = simple_plot_data()$size
            
            diff_dip = abs(simple_size - diploid$size)
            diff_hap = abs(simple_size - haploid$size)
            
            if (diff_dip < diff_hap) {
                return(diploid)
            } else {
                return(haploid)
            }
            
        } else if (input$genome_type == "diploid") {
            num_peaks = 2
        } else {
            num_peaks = 1
        }
        return(peak_count_kmer(df, input$peak_min_kmer, input$peak_max_kmer, show_error = show, num_peaks = num_peaks))
    })
    
    gscope_data = reactive({
        df <- reactive_df()
        max_kmer = if (is.null(input$gscope_max_kmer)) 100 else input$gscope_max_kmer
        r = runGenomeScope(df, input$kmer_length, input$read_length, max_kmer, input$gscope_num_rounds, input$gscope_start_shift,
                           input$gscope_error_cutoff, input$gscope_max_iter, input$gscope_score_close, input$gscope_het_diff)
        return(r)
    })
    
    cutoff_sizes <- eventReactive(reactive_df(), {
        validate(
            need(input$render_cutoff_plot, "Please Generate Report")
        )
        withProgress(message = 'Calculation in progress',
            detail = 'Calculating size estimations for each model ', value = 0, {
                df <- reactive_df()
                max <- max(df$Frequency)
                gscope = c()
                simple = c()
                peak = c()
                num_iter = 33
                i = 1
                # perc = c(1/max, 0.01, seq(0.05, 0.5, 0.05), 1)
                cutoffs = c(100, input$simple_max_kmer, input$peak_max_kmer, input$gscope_max_kmer, max - 1, max)
                cutoffs = sort(unique(cutoffs))
                n = length(cutoffs)
                         
                for (i in 1:n) {
                    max_kmer = cutoffs[i]
                             
                    g = runGenomeScope(df, input$kmer_length, input$read_length, max_kmer, input$gscope_num_rounds,
                                       input$gscope_start_shift, input$gscope_error_cutoff, input$gscope_max_iter,
                                       input$gscope_score_close, input$gscope_het_diff)
                    s = simple_count_kmer(df, input$min_kmer, max_kmer, show_error=TRUE)
                    p = peak_count_kmer(df, input$min_kmer, max_kmer, show_error=TRUE, num_peaks=1)
                             
                    gscope[[i]] = if (g$size > 0) as.integer(g$size) else NULL
                    simple[[i]] = if (s$size) as.integer(s$size) else NULL
                    peak[[i]] = if (p$size) as.integer(p$size) else NULL
                             
                    incProgress(1/n, detail = paste(i, "/", n))
                }
        })
        return(list("data" = data.frame(cutoffs, gscope, peak, simple), "title" = filename()$name))
    })
    ##################################################################################################start
    ############################    batchAnalysis  ########  K-mer profiles     ###########################
    ########################################################################################################
    
  
    #####IF upload jellyfish kmer profiles
    #####summary of uploading files
    files_summary <- reactive({
        inFile <- input$kmer_files
        validate(
            need(inFile, "Please upload one or more jellyfish kmer profile(s)  OR upload a summary csv file." ) #batch Analysis page main panel
        )

        if (is.null(inFile))
            return(NULL)
        Filesname<- regmatches(inFile$name,regexpr(".*",inFile$name))
        Kmer_list <- c()
        ReadLength_list <- c()
        MaxCutoff_list <- c()

        for (file in Filesname){

            Kmer<- regmatches(file,regexpr("[k][0-9][0-9]*",file))
            if (length(Kmer) == 0 ){Kmer <- "k21"}else{Kmer<- regmatches(file,regexpr("[k][0-9][0-9]*",file))}
            Kmer_list <- c(Kmer_list,Kmer)


            ReadLength<- regmatches(file,regexpr("[r][0-9]+.[0-9]",file))
            if (length(ReadLength) == 0 ){ ReadLength <- "r149.0"}else{ReadLength<- regmatches(file,regexpr("[r][0-9]+.[0-9]",file))}
            ReadLength_list <- c(ReadLength_list,ReadLength)


            MaxCutoff<- regmatches(file,regexpr("[0-9]+[k]",file))
            if (length(MaxCutoff) == 0 ){ MaxCutoff <- "10k"}else{MaxCutoff<- regmatches(file,regexpr("[0-9]+[k]",file))}
            MaxCutoff_list <- c(MaxCutoff_list,MaxCutoff)
        }

        filematrix <- data.frame(Filesname, Kmer_list, ReadLength_list, MaxCutoff_list)
        colnames(filematrix) <- c("FileName", "Kmer", "ReadLength", "MaxCutoff")

        return(filematrix)

    })

    batchAnalysis <- reactive({
        validate(
            need(input$kmer_files, "Please upload one or more jellyfish kmer profile(s)  OR upload a summary csv file.") #batch Analysis page main panel
        )

        filepaths = input$kmer_files$datapath

        filenames = input$kmer_files$name

        sizes = c()
        stats = c()

        withProgress(message = "Estimating sizes", value = 0, {
            n = length(filepaths)

            for (i in 1:n) {
                validate(
                    need(try(df <- read.table(filepaths[i])), paste("Could not read file: ", filenames[i])),
                    need(ncol(df) == 2, paste(filenames[i], " does not have 2 columns"))
                )

                names(df) = c("Frequency", "Count")
                rownames(df) = df$Frequency

                #######extract kmer /read_length/ max_cutoff from table
                files_summary <- files_summary()
                max_cutoff <- regmatches(files_summary$MaxCutoff[i],regexpr("\\-*\\d+\\.*\\d*",files_summary$MaxCutoff[i]))
                max_cutoff <- as.numeric(max_cutoff)*1000
                kmer <- regmatches(files_summary$Kmer[i],regexpr("\\-*\\d+\\.*\\d*",files_summary$Kmer[i]))
                kmer <- as.numeric(kmer)
                readlength <- regmatches(files_summary$ReadLength[i],regexpr("\\-*\\d+\\.*\\d*",files_summary$ReadLength[i]))
                readlength <- as.numeric(readlength)

                rs = simple_count_kmer(df, input$batch_min_kmer, max_cutoff)
                rp = peak_count_kmer(df, input$batch_min_kmer, max_cutoff)
                rg = runGenomeScope(df, kmer, readlength, max_cutoff,
                                    input$batch_gscope_num_rounds, input$batch_gscope_start_shift, input$batch_gscope_error_cutoff,
                                    input$batch_gscope_max_iter, input$batch_gscope_score_close, input$batch_gscope_het_diff)

                sizes = c(sizes, c(rg$size, rp$size, rs$size))
                stats = c(stats, as.vector(rg$summary$Maximum))

                incProgress(1/n, detail = paste(i, "/", n))
            }
        })


        sizes = matrix(sizes, ncol = 3, byrow = TRUE)
        colnames(sizes) = c("GenomeScope", "Peak Frequency", "Simple Count")
        sizes = as.data.frame(sizes)
        sizes = add_column(sizes, File = filenames, .before = "GenomeScope")

        stats = matrix(stats, ncol = 6, byrow = TRUE)
        colnames(stats) = c("Heterozygosity", "Genome Haploid Length", "Genome Repeat Length",
                                   "Genome Unique Length", "Model Fit", "Read Error Rate")
        stats = as.data.frame(stats)
        stats = add_column(stats, File = filenames, .before = "Heterozygosity")

        return(list("sizes" = sizes, "stats" = stats))
    })


    




    ###############################Batch analysis output tables###############################
    ###If upload kmer profiles
    output$batch_files_table <- renderDataTable({ files_summary() }) #summary of files
    ###If upload a csv file
    output$new_csv <- renderDataTable({ new_csv() }) # modified csv file
    
    output$batch_sizes_table = renderDataTable(      #size prediction
        {
            r = batchAnalysis()
            r$sizes
        }
    )

    output$batch_stats_table = renderDataTable(     #GenomoeScope Statistic
        {
            r = batchAnalysis()
            r$stats
        }
    )

    output$batch_summary_csv <- downloadHandler(       #download summary as csv
        filename = function() {
            paste("batch-summary-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            filesummary= files_summary()
            write.csv(filesummary, file, row.names = FALSE)
        }
    )


    output$batch_size_csv <- downloadHandler(       #download size prediction as csv
        filename = function() {
            paste("batch-sizes-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            r = batchAnalysis()
            write.csv(r$sizes, file, row.names = FALSE)
        }
    )

    output$batch_stats_csv <- downloadHandler(     #download genomeScope statistic as csv
        filename = function() {
            paste("batch-stats-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            r = batchAnalysis()
            write.csv(r$stats, file, row.names = FALSE)
        }
    )
    
 
    
    
    ##################################################################################################start#
    ############################    batchAnalysis  ########  K-mer profiles + csv file     ################
    ########################################################################################################
    
    
    #####IF upload jellyfish kmer profiles
    #####summary of uploading files
    ##########IF upload csv file
    new_csv <- reactive({
        inFile <- input$csv_file
         if (is.null(inFile))
             return(NULL)
        filematrix <- read.csv(inFile$datapath)
        #print(filematrix)
        return(filematrix)
    })
    
    
    batch_csv_Analysis <- reactive({
        validate(
            need(input$kmer_files_1, "Please upload one or more jellyfish kmer profile(s)  AND  a summary csv file.") #batch Analysis page main panel
        )

        filepaths = input$kmer_files_1$datapath
        filenames = input$kmer_files_1$name

        sizes = c()
        stats = c()

        withProgress(message = "Estimating sizes", value = 0, {
            n = length(filepaths)

            for (i in 1:n) {
                validate(
                    need(try(df <- read.table(filepaths[i])), paste("Could not read file: ", filenames[i])),
                    need(ncol(df) == 2, paste(filenames[i], " does not have 2 columns"))
                )

                names(df) = c("Frequency", "Count")
                rownames(df) = df$Frequency

                #######extract kmer /read_length/ max_cutoff from table
                files_summary <- new_csv()
                max_cutoff <- regmatches(files_summary$MaxCutoff[i],regexpr("\\-*\\d+\\.*\\d*",files_summary$MaxCutoff[i]))
                max_cutoff <- as.numeric(max_cutoff)*1000
                kmer <- regmatches(files_summary$Kmer[i],regexpr("\\-*\\d+\\.*\\d*",files_summary$Kmer[i]))
                kmer <- as.numeric(kmer)
                readlength <- regmatches(files_summary$ReadLength[i],regexpr("\\-*\\d+\\.*\\d*",files_summary$ReadLength[i]))
                readlength <- as.numeric(readlength)

                rs = simple_count_kmer(df, input$batch_min_kmer_1, max_cutoff)
                rp = peak_count_kmer(df, input$batch_min_kmer_1, max_cutoff)
                rg = runGenomeScope(df, kmer, readlength, max_cutoff,
                                    input$batch_gscope_num_rounds_1, input$batch_gscope_start_shift_1, input$batch_gscope_error_cutoff_1,
                                    input$batch_gscope_max_iter_1, input$batch_gscope_score_close_1, input$batch_gscope_het_diff_1)

                sizes = c(sizes, c(rg$size, rp$size, rs$size))
                stats = c(stats, as.vector(rg$summary$Maximum))

                incProgress(1/n, detail = paste(i, "/", n))
            }
        })


        sizes = matrix(sizes, ncol = 3, byrow = TRUE)
        colnames(sizes) = c("GenomeScope", "Peak Frequency", "Simple Count")
        sizes = as.data.frame(sizes)
        sizes = add_column(sizes, File = filenames, .before = "GenomeScope")

        stats = matrix(stats, ncol = 6, byrow = TRUE)
        colnames(stats) = c("Heterozygosity", "Genome Haploid Length", "Genome Repeat Length",
                            "Genome Unique Length", "Model Fit", "Read Error Rate")
        stats = as.data.frame(stats)
        stats = add_column(stats, File = filenames, .before = "Heterozygosity")

        return(list("sizes" = sizes, "stats" = stats))
    })
    
    
    
    
    
    
    
    ###############################Batch analysis output tables###############################
    ###If upload a csv file
    output$new_csv <- renderDataTable({ new_csv() }) # modified csv file
    
    output$batch_sizes_table_1 <- renderDataTable(      #size prediction
        {
            r = batch_csv_Analysis()
            r$sizes
        }
    )
    
    output$batch_stats_table_1 <- renderDataTable(     #GenomoeScope Statistic
        {
            r = batch_csv_Analysis()
            r$stats
        }
    )
    

    
    
    output$batch_size_csv_1 <- downloadHandler(       #download size prediction as csv
        filename = function() {
            paste("batch-sizes-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            r = batch_csv_Analysis()
            write.csv(r$sizes, file, row.names = FALSE)
        }
    )
    
    output$batch_stats_csv_1 <- downloadHandler(     #download genomeScope statistic as csv
        filename = function() {
            paste("batch-stats-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            r = batch_csv_Analysis()
            write.csv(r$stats, file, row.names = FALSE)
        }
    )
    
    
    
    
    
    
    
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
    
    #
    # Generate outputs
    #
    
    # Toggles genomeScope summary table
    observeEvent(gscope_data(), {
        r = gscope_data()
        if (r$size != -1 && input$plot_type == "gscope")
            shinyjs::show("gscope_summary")
        else
            shinyjs::hide("gscope_summary")
    })
    observeEvent(input$plot_type, {
        r = gscope_data()
        if (r$size != -1 && input$plot_type == "gscope")
            shinyjs::show("gscope_summary")
        else
            shinyjs::hide("gscope_summary")
    })
    
    # if file change hide output
    observeEvent(reactive_df(), {
        updateButton(session, "render_cutoff_plot", value=FALSE)
    })
    
    # MIN KMER SLIDER TEXT INPUTS
    # simple
    observeEvent(input$simple_min_kmer_text, {
        updateSliderInput(session, "simple_min_kmer", value = input$simple_min_kmer_text)
    })
    
    output$simple_minkmer_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        
        fluidRow(
            column(width = 8,
                   sliderInput("simple_min_kmer_text", "Minimum kmer cutoff",
                               min = 1, max = max_freq, value = calc_start_freq(df), step = as.integer(0.1*max_freq)
                   )
            ),
            column(width = 4,
                   numericInput("simple_min_kmer", label = "", value = calc_start_freq(df))    
            )
        )
    })
    
    # peak
    observeEvent(input$peak_min_kmer_text, {
        updateSliderInput(session, "peak_min_kmer", value = input$peak_min_kmer_text)
    })
    
    output$peak_minkmer_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        
        fluidRow(
            column(width = 8,
                   sliderInput("peak_min_kmer_text", "Minimum kmer cutoff",
                               min = 1, max = max_freq, value = calc_start_freq(df), step = as.integer(0.1*max_freq)
                   )
            ),
            column(width = 4,
                   numericInput("peak_min_kmer", label = "", value = calc_start_freq(df))    
            )
        )
    })
    
    # MAX KMER SLIDER TEXT INPUTS
    # simple
    observeEvent(input$simple_max_kmer_text, {
        updateSliderInput(session, "simple_max_kmer", value = input$simple_max_kmer_text)
    })
    
    output$simple_maxkmer_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        
        fluidRow(
            column(width = 8,
                   sliderInput("simple_max_kmer", "Maximum kmer cutoff",
                               min = 1, max = max_freq, value = 100, step = as.integer(0.1*max_freq)
                   )
            ),
            column(width = 4,
                   numericInput("simple_max_kmer_text", label = "", value = 100)    
            )
        )
    })
    
    # peak
    observeEvent(input$peak_max_kmer_text, {
        updateSliderInput(session, "peak_max_kmer", value = input$peak_max_kmer_text)
    })
    
    output$peak_maxkmer_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        
        fluidRow(
            column(width = 8,
                   sliderInput("peak_max_kmer", "Maximum kmer cutoff",
                               min = 1, max = max_freq, value = 100, step = as.integer(0.1*max_freq)
                   )
            ),
            column(width = 4,
                   numericInput("peak_max_kmer_text", label = "", value = 100)    
            )
        )
    })
    
    # gscope
    observeEvent(input$gscope_max_kmer_text, {
        updateSliderInput(session, "gscope_max_kmer", value = input$gscope_max_kmer_text)
    })
    
    output$gscope_maxkmer_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        
        fluidRow(
            column(width = 8,
                   sliderInput("gscope_max_kmer", "Maximum kmer cutoff",
                               min = 1, max = max_freq, value = 100, step = as.integer(0.1*max_freq)
                   )
            ),
            column(width = 4,
                   numericInput("gscope_max_kmer_text", label = "", value = 100)    
            )
        )
    })
    
    # generate results
    output$plot = renderPlotly({
        if (input$plot_type == "gscope") {
            r = gscope_data()
            if (r$size == -1)
                showNotification("GenomeScope failed to converage", duration = NULL, id = "gscope_error",
                    type = "warning")
            else
                removeNotification("gscope_error")
            if (input$gscope_type == "linear")
                r$linear_plot
            else
                r$log_plot
        } else if (input$plot_type == "simple") {
            r = simple_plot_data()
            r$graph
        } else if (input$plot_type == "peak") {
            r = peak_plot_data()
            r$graph
        }
    })
    
    size_table <- reactive({
        denom <- 1000000
        rs <- round(simple_plot_data()$size / denom, 2)
        rp <- round(peak_plot_data()$size / denom, 2)
        rg <- if (gscope_data()$size != -1) round(gscope_data()$size / denom, 2) else "N/A"
        
        outdf <- data.frame(
            Method=c("Simple Count", "Peak Frequency", "Genome Scope"),
            Size=c(rs, rp, rg)
        )
        colnames(outdf) <- c("Model", "Size (MB)")
        outdf
    })
    
    output$size_table <- renderTable({
        size_table()
    })
    
    output$simple_size <- renderText({
        r <- simple_plot_data()
        r$size
    })
    
    output$freq_size <- renderText({
        r <- peak_plot_data()
        r$size
    })
    
    output$gscope_summary <- renderTable(rownames = TRUE, {
        r = gscope_data()
        r$summary
    })
    
    output$gscope_size <- renderText({
        r = gscope_data()
        r$size
    })
    
    output$summary <- renderText({
        settings()
    })
    
    
    #####################################resultsPage########################################start
    output$cutoff_plot <- renderPlotly({
        df <- cutoff_sizes()
        title <- df$title
        data <- df$data
        
        p = plot_ly(data, x= ~cutoffs, y= ~gscope,
                    name = "Genome Scope", type="scatter", mode="lines")
        p = add_trace(p, x= ~cutoffs, y= ~peak,
                      name = "Peak Kmer", type="scatter", mode="lines")
        p = add_trace(p, x= ~cutoffs, y= ~simple,
                      name = "Simple Count", type="scatter", mode="lines")
        p = layout(p, title=title, showlegend = TRUE, xaxis=list(title='Max kmer cutoff'), yaxis=list(title='Genome Size'))
        p$elementId = NULL  #TODO temp approach to suppress warning
        return(p)
    })
    
    output$cutoff_table <- renderTable({
        values <- cutoff_sizes()$data
        return(values)
    })
    
    
    #####################################resultsPage########################################end
    
    
   
    
    ################################################################################################## batchAnalysis end
    
    #################################main page########################################################start
    output$downloadGscope <- downloadHandler(
        filename = function() {
            name=filename()
            paste(name, "gScope_Output", ".csv", sep = "")
        },
        content = function(file){
            r = gscope_data()
            write.csv(r$summary, file)
        }
    )
    
    output$downloadSize <- downloadHandler(
        filename = function() {
            name=filename()
            paste(name, "Size_Output", ".csv", sep = "")
        },
        content = function(file){
            r = size_table()
            r$kmer_length = c(input$kmer_length, input$kmer_length, input$kmer_length)
            r$read_length = c(input$read_length, input$read_length, input$read_length)
            
            
            if(is.null(input$simple_min_kmer) || is.null(input$simple_max_kmer)) {
                simple_min = calc_start_freq(reactive_df())
                simple_max = 100
            } else {
                simple_min = input$simple_min_kmer
                simple_max = input$simple_max_kmer
            }
            
            if(is.null(input$peak_min_kmer) || is.null(input$peak_max_kmer)) {
                peak_min = calc_start_freq(reactive_df())
                peak_max = 100
            } else {
                peak_min = input$peak_min_kmer
                peak_max = input$peak_max_kmer
            }
            
            if(is.null(input$gscope_min_kmer) || is.null(input$gscope_max_kmer)) {
                gscope_min = 15
                gscope_max = 100
            } else {
                gscope_min = input$gscope_min_kmer
                gscope_max = input$gscope_max_kmer
            }
            
            r$error_cutoff = c(simple_min, peak_min, gscope_min)
            r$kmer_cutoff = c(simple_max, peak_max, gscope_max)
            
            colnames(r) <- c("Method","Size", "Kmer Length", "Read Length", "Error Cutoff", "Kmer Cutoff")
            write.csv(r, file)
        }
    )
    #################################main page########################################################end
    
    
    
    
    # https://beta.rstudioconnect.com/content/2671/Combining-Shiny-R-Markdown.html#generating_downloadable_reports_from_shiny_app
    # http://shiny.rstudio.com/gallery/download-knitr-reports.html
    output$report <- downloadHandler(
        
        
        # For PDF output, change this to "report.pdf"
        filename = function() {
            name = paste(filename()$name, "Report", sep="_")
            paste(name, sep=".",
                  switch(input$report_format,
                         PDF = "pdf", HTML = "html", Word = "doc"
                  )
            )
        },
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            df = reactive_df()
            params <- list(df = df,
                           input_type = input$type,
                           simple_min_kmer = input$simple_min_kmer,
                           peak_min_kmer = input$peak_min_kmer,
                           simple_max_kmer = input$simple_max_kmer,
                           peak_max_kmer = input$peak_max_kmer,
                           gscope_max_kmer = input$gscope_max_kmer,
                           diploid = input$genome_type,
                           show_hide = input$show_hide_button,
                           kmer_length = input$kmer_length, 
                           read_length = input$read_length,
                           gscope_rounds = input$gscope_num_rounds, 
                           gscope_start_shift = input$gscope_start_shift,
                           gscope_error_cuttoff = input$gscope_error_cutoff,
                           gscope_max_iter = input$gscope_max_iter,
                           gscope_score_close = input$gscope_score_close, 
                           gscope_het_diff = input$gscope_het_diff
            )
            
            
            
            
            
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            out <- rmarkdown::render(tempReport, output_file = file,
                                     output_format = switch(input$report_format,
                                                            PDF = "pdf_document", HTML = "html_document", Word = "word_document"
                                     ),
                                     params = params,
                                     envir = new.env(parent = globalenv())
            )
            file.rename(out, file)
        }
    )
    
    
    
    ######History Page#####################################################################################start
    
    #This function is repsonsible for loading in the selected file
    filedata1 <- reactive({
        infile1 <- input$datafile1
        if (is.null(infile1)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        read.csv(infile1$datapath)
    })
    
    
    filedata2 <- reactive({
        infile2 <- input$datafile2
        if (is.null(infile2)) {
            # User has not uploaded a file yet
            return(NULL)
        }
        read.csv(infile2$datapath)
    })
    
    #This previews the CSV data file
    output$filetable1 <- renderDataTable({
        filedata1()
    })
    output$filetable2 <- renderDataTable({
        filedata2()
    })
    ######History Page#####################################################################################end
    
    
})
