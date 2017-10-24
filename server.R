library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(tools)
library(knitr)
source("simpleCountKmer.R")     # functions to estimate genome size
source("peakCountKmer.R")
source("genomeScope.R")
source("serverHelpers.R")       # helper functions used in server.R
source("simulation.R")          # simulates frequency/count data

shinyServer(function(input, output, session) {
    
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
    
    
    #
    # Object/Event listeners
    #

    observeEvent(input$gscope_adv_toggle, {
        if (input$gscope_adv_toggle == TRUE)
            shinyjs::show("gscope_adv_settings", anim = TRUE)
        else
            shinyjs::hide("gscope_adv_settings", anim = TRUE)
    })
    
    observeEvent(input$gscope_batch_toggle, {
        if (input$gscope_batch_toggle == TRUE)
            shinyjs::show("gscope_batch_settings", anim = TRUE)
        else
            shinyjs::hide("gscope_batch_settings", anim = TRUE)
    })
    
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
        } else {
            validate(
                need(FALSE, "Simulation unavailable")
            )
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
                           input$sim_kmer_length, input$sim_heterozygosity / 100)
        }

        toggle_settings(show = init_elems, anim = TRUE, anim_type = "fade")
        names(df) = c("Frequency", "Count")
        rownames(df) = df$Frequency
        return(df)
    })
    
    # generate plots and size estimates
    simple_plot_data <- reactive({
        df <- reactive_df()
        if (is.null(input$min_kmer) || is.null(input$max_kmer)) {
            r = simple_count_kmer(df, show_error=FALSE)
        } else {
            if (is.null(input$show_hide_button) || input$show_hide_button == "hide_error") {
                show = FALSE
            } else if (input$show_hide_button == "show_error") {
                show = TRUE
            }
            
            r = simple_count_kmer(df, input$min_kmer, input$max_kmer, show_error=show)
        }
        return(r)
    })
    
    peak_plot_data <- reactive({
        df <- reactive_df()
        if (is.null(input$min_kmer) || is.null(input$max_kmer)) {
            r = peak_count_kmer(df, show_error=FALSE)
        } else {
            if (is.null(input$show_hide_button) || input$show_hide_button == "hide_error") {
                show = FALSE
            } else if (input$show_hide_button == "show_error") {
                show = TRUE
            }
            
            if (input$genome_type == "diploid") {
                num_peaks = 2
            } else {
                num_peaks = 1
            }
            r = peak_count_kmer(df, input$min_kmer, input$max_kmer, show_error = show, num_peaks = num_peaks)
        }
        return(r)
    })
    
    gscope_data = reactive({
        df <- reactive_df()
        max_kmer = if (is.null(input$max_kmer)) 100 else input$max_kmer
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
                cutoff = c()
                gscope = c()
                simple = c()
                peak = c()
                num_iter = 33
                i = 1
                perc = c(1/max, 0.01, seq(0.05, 0.5, 0.05), 1)
                for (x in perc) {
                    max_kmer = as.integer(x*max)
                    cutoff[[i]] = max_kmer
                    
                    g = runGenomeScope(df, input$kmer_length, input$read_length, max_kmer, input$gscope_num_rounds, input$gscope_start_shift,
                                       input$gscope_error_cutoff, input$gscope_max_iter, input$gscope_score_close, input$gscope_het_diff)
                    incProgress(1/num_iter)
                    s = simple_count_kmer(df, input$min_kmer, max_kmer, show_error=TRUE)
                    incProgress(1/num_iter)
                    p = peak_count_kmer(df, input$min_kmer, max_kmer, show_error=TRUE, num_peaks=1)
                    incProgress(1/num_iter)
                    
                    gscope[[i]] = if (g$size > 0) as.integer(g$size) else NULL
                    simple[[i]] = if (s$size) as.integer(s$size) else NULL
                    peak[[i]] = if (p$size) as.integer(p$size) else NULL
                    i = i+1
                }
        })
        return(list("data" = data.frame(cutoff, perc, gscope, peak, simple), "title" = filename()$name))
    })
    
    batchAnalysis = reactive({
        validate(
            need(input$kmer_files, "Please upload one or more jellyfish kmer profile(s)"),
            need(input$kmer_length <= input$read_length, "Kmer-length cannot be greater than read length")
        )
        
        filepaths = input$kmer_files$datapath
        filenames = input$kmer_files$name
        
        sizes = c()
        stats = c()
        
        for (i in 1:length(filepaths)){
            validate(
                need(try(df <- read.table(filepaths[i])), paste("Could not read file: ", filenames[i])),
                need(ncol(df) == 2, paste(filenames[i], " does not have 2 columns"))
            )
            
            names(df) = c("Frequency", "Count")
            rownames(df) = df$Frequency
            
            rs = simple_count_kmer(df, input$batch_min_kmer, input$batch_max_kmer)
            rp = peak_count_kmer(df, input$batch_min_kmer, input$batch_max_kmer)
            rg = runGenomeScope(df, input$kmer_length, input$read_length, input$batch_max_kmer, input$gscope_num_rounds, 
                                input$gscope_start_shift, input$gscope_error_cutoff, input$gscope_max_iter, input$gscope_score_close, 
                                input$gscope_het_diff)
            
            sizes = c(sizes, c(rg$size, rp$size, rs$size))
            stats = c(stats, as.vector(rg$summary$Maximum))
        }
        
        shinyjs::show("batch_size_header")
        shinyjs::show("batch_stats_elems")
        
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
    
    # link numeric and slider inputs
    observeEvent(input$min_kmer, {
        isolate(updateNumericInput(session, "min_kmer_numeric", value = input$min_kmer))
    })
    observeEvent(input$min_kmer_numeric, {
        isolate(updateSliderInput(session, "min_kmer", value = input$min_kmer_numeric))
    })
    
    output$minkmer_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        
        fluidRow(
            column(width = 8,
                   sliderInput("min_kmer_numeric", "Minimum kmer cutoff",
                               min = 1, max = max_freq, value = calc_start_freq(df), step = as.integer(0.1*max_freq)
                   )
            ),
            column(width = 4,
                   numericInput("min_kmer", label = "",
                                min = 1, max = max_freq, value = calc_start_freq(df), step = 1
                   )    
            )
        )
    })
    
    # link numeric and slider inputs
    observeEvent(input$max_kmer, {
        if (input$max_kmer != input$max_kmer_numeric) {
            updateNumericInput(session, "max_kmer_numeric", value = isolate(input$max_kmer))
        }
    })
    observeEvent(input$max_kmer_numeric, {
        if (input$max_kmer != input$max_kmer_numeric) {
            updateSliderInput(session, "max_kmer", value = isolate(input$max_kmer_numeric))
        }
    })
    
    output$maxkmer_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        
        fluidRow(
            column(width = 8,
                   sliderInput("max_kmer", "Maximum kmer cutoff",
                               min = 1, max = max_freq, value = 100, step = as.integer(0.1*max_freq)
                   )
            ),
            column(width = 4,
                   numericInput("max_kmer_numeric", label = "",
                                min = 1, max = max_freq, value = 100, step = 1
                   )    
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
    
    
    output$size_table <- renderTable({
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

    settings <- reactive({
        if (input$type == "file") {
            input_widgets
        } else if (input$type == "simulation") {
            all_sim_widgets = c("sim_genome_size", "sim_genome_type", "sim_heterozygosity")
        } else {
            sample_widgets
        }
    })

    output$summary <- renderText({
        settings()
    })
    
    output$cutoff_plot <- renderPlotly({
        df <- cutoff_sizes()
        title <- df$title
        data <- df$data
        
        p = plot_ly(data, x= ~cutoff, y= ~gscope,
                    name = "Genome Scope", type="scatter", mode="lines")
        p = add_trace(p, x= ~cutoff, y= ~peak,
                      name = "Peak Kmer", type="scatter", mode="lines")
        p = add_trace(p, x= ~cutoff, y= ~simple,
                      name = "Simple Count", type="scatter", mode="lines")
        p = layout(p, title=title, showlegend = TRUE, xaxis=list(title='Max kmer cutoff'), yaxis=list(title='Genome Size'))
        p$elementId = NULL  #TODO temp approach to suppress warning
        return(p)
    })
    
    output$cutoff_table <- renderTable({
        values <- cutoff_sizes()$data
        return(values)
    })
    
    output$batch_sizes_table = renderTable(
        {
            r = batchAnalysis()
            r$sizes
        },
        bordered = TRUE,
        hover = TRUE
    )
    
    output$batch_stats_table = renderTable(
        {
            r = batchAnalysis()
            r$stats
        },
        bordered = TRUE,
        hover = TRUE
    )
    
    
    # https://beta.rstudioconnect.com/content/2671/Combining-Shiny-R-Markdown.html#generating_downloadable_reports_from_shiny_app
    # http://shiny.rstudio.com/gallery/download-knitr-reports.html
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = function() {
            paste("test", sep=".",
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
                           min_kmer = input$min_kmer,
                           max_kmer = input$max_kmer,
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
})
