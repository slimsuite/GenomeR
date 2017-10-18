library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
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
    toggle_sim_widgets = c("sim_genome_size", "sim_genome_type")
    sample_widgets = c("sample")
    
    all_settings = c("minkmer_slider", "maxkmer_slider", "genome_type", "show_hide_button", "gscope_type", "gscope_summary")
    genomescope_set = c("maxkmer_slider",  "gscope_type", "gscope_summary")
    simplecount_set = c("minkmer_slider", "maxkmer_slider", "show_hide_button")
    peakfreq_set = c("minkmer_slider", "maxkmer_slider", "genome_type", "show_hide_button")
    
    #
    # Initial conditions
    #

    output$input_summary <- get_output_summary(input, input_widgets)
    
    
    #
    # Object/Event listeners
    #

    
    # listener to enable heterozygosity only for diploid genomes
    observe({
        toggle_heterozygosity(input)
    })

    observeEvent(input$kmer_length, {
        if (input$kmer_length > input$read_length) {
            showNotification("Kmer-length cannot be greater than read length", type="error")
        }
    })

    observeEvent(input$read_length, {
        updateNumericInput(session, "kmer_length", max = input$read_length)
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
                need(input$kmer_file, "Please upload a jellyfish kmer profile")
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
            df <- simulate()
        }
        
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
        max = if (is.null(input$max_kmer)) -1 else input$max_kmer
        r = runGenomeScope(df, input$kmer_length, input$read_length, max, input$gscope_num_rounds, input$gscope_start_shift,
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
                    
                    g = runGenomeScope(df, input$kmer_length, input$read_length, max_kmer)
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
    
    
    #
    # Generate outputs
    #
    
    # if file change hide output
    observeEvent(reactive_df(), {
        updateButton(session, "render_cutoff_plot", value=FALSE)
    })
    
    # link numeric and slider inputs
    observeEvent(input$min_kmer, {
        isolate(updateSliderInput(session, "min_kmer_numeric", value = input$min_kmer))
    })
    observeEvent(input$min_kmer_numeric, {
        isolate(updateNumericInput(session, "min_kmer", value = input$min_kmer_numeric))
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
        isolate(updateSliderInput(session, "max_kmer_numeric", value = input$max_kmer))
    })
    observeEvent(input$max_kmer_numeric, {
        isolate(updateNumericInput(session, "max_kmer", value = input$max_kmer_numeric))
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
        rs <- simple_plot_data()$size / denom
        rp <- peak_plot_data()$size / denom
        rg <- gscope_data()$size / denom

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
