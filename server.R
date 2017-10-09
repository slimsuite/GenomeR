library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(plotly)
source("simpleCountKmer.R")     # functions to estimate genome size
source("peakCountKmer.R")
source("genomeScope.R")
source("serverHelpers.R")       # helper functions used in server.R

shinyServer(function(input, output, session) {
    
    #
    # Setup variables and any intermediary/conductor function
    #
    
    input_widgets = c("kmer_file", "kmer_length", "read_length", "max_kmer_coverage")
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
    
    # update freq slider based on max kmer coverage numeric input
    observe({
        updateNumericInput(session, "max_kmer_coverage", value = input$max_kmer)
    })
    
    observe({
        updateNumericInput(session, "max_kmer", value = input$max_kmer_coverage)
    })
    
    # if new file reset max kmer cutoff
    observeEvent(reactive_df(), {
        updateSliderInput(session, "max_kmer", value = 1000)
    })
    
    
    
    #
    # Reactive values
    #
    
    filename <- reactive({
        if (input$type == "file") {
            # check we actually have a file
            validate(
                need(input$kmer_file, "Please upload a jellyfish kmer profile")
            )
            return(input$kmer_file$datapath)
        } else if (input$type == "sample") {
            validate(
                need(file.exists(input$sample), "Sample doesn't exist")
            )
            return(input$sample)
        } else {
            validate(
                need(FALSE, "Simulation unavailable")
            )
        }
    })
    
    # open file and save into data frame
    reactive_df <- reactive({
        file <- filename()
        df <- read.table(file)
        
        # validate the data frame
        validate(
            need(df, paste("Could not read file: ", file)),
            need(ncol(df) == 2, "File does not have 2 columns")
        )
        
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
        r = runGenomeScope(df, input$kmer_length, input$read_length, input$max_kmer_coverage)
        return(r)
    })
    
    
    #
    # Generate outputs
    #
    
    output$minkmer_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        val <- input$min_kmer
        
        # set initial value
        if (is.null(val)) {
            val <- calc_start_freq(df)
        }
        
        sliderInput("min_kmer", "Minimum kmer cutoff",
            min = 0, max = max_freq, value = val, step = 1
        )
    })
    
    output$maxkmer_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        val <- input$max_kmer
        
        # set initial val to max, otherwise keep current value
        if (is.null(val)) {
            val <- max_freq
        }
        
        minimum = input$min_kmer
        if (input$plot_type == "gscope") {
            minimum = 1
        }
        
        # create slider
        sliderInput("max_kmer", "Maximum kmer cutoff",
            min = minimum, max = max_freq, value = val, step = 1
        )
    })
    
    # generate results
    output$plot = renderPlotly({
        if (input$plot_type == "gscope") {
            r = gscope_data()
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
        } else if (input$type == "sample") {
            all_sim_widgets = c("sim_genome_size", "sim_genome_type", "sim_heterozygosity")
        } else {
            sample_widgets
        }
    })
    
    output$summary <- renderText({
        settings()
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
            params <- list(n = input$kmer_length)
            
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
