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
    all_sim_widgets = c("sample", "sim_genome_size", "sim_genome_type", "sim_heterozygosity")
    toggle_sim_widgets = c("sample", "sim_genome_size", "sim_genome_type")
    
    
    
    #
    # Initial conditions
    #
    
    # disable output by default
    disable_output()
    
    # disable simulation by default
    toggle_widgets(toggle_sim_widgets, FALSE)
    output$input_summary <- get_output_summary(input, input_widgets)
    
    # disable type - only allow user input for now
    # disable("type")
    

    
    #
    # Object/Event listeners
    #
    
    # if switching to user input switch focus, disable simulation and enable input settings
    observeEvent(input$type, {
        if (input$type == "File input") {
            toggle_widgets(toggle_sim_widgets, FALSE)
            toggle_widgets(input_widgets, TRUE)
            removeClass("input-col", "dim")
            addClass("sim-col", "dim")
            output$input_summary <- get_output_summary(input, input_widgets)
        } else {
            toggle_widgets(toggle_sim_widgets, TRUE)
            toggle_widgets(input_widgets, FALSE)
            addClass("input-col", "dim")
            removeClass("sim-col", "dim")
            output$input_summary <- get_output_summary(input, all_sim_widgets)
        }
    })
    
    # listener to enable heterozygosity only for diploid genomes
    observe({
        toggle_heterozygosity(input)
    })
    
    # navigate to the results page on input submition
    # TODO input checking
    observeEvent(input$submit, {
        disable_output()

        #checks file has been selected
        if (input$type == "File input") {
            if (is.null(input$kmer_file)) {
                showNotification("Please upload a kmer profile", type="error")
                return(FALSE)
            }

            tryCatch(data <- read.table(input$kmer_file$datapath),
                error=function(error_message) {
                    showNotification(
                        paste(
                            "File not in readable table format: ",
                            error_message
                        ),
                        type="error"
                    )
                    return(NA)
                }
            )

            if (ncol(data) != 2) {
                showNotification("File has more than 2 columns", type="error")
                return(FALSE)
            }

        } else if (input$type == "Simulation input" && input$sample == "Select sample") {
            showNotification("Simulation currently unavailable", type="error")
            return(FALSE)
        }

        # checks passed, move to output page
        enable_output()
        updateNavbarPage(session, "navbar", "nav_output")
        return(TRUE)
    })
    


    #
    # Reactive values
    #

    filename <- reactive({
        if (input$type == "File input") {
            # check we actually have a file
            validate(
                need(input$kmer_file, "Please upload a jellyfish kmer profile")
            )
            return(input$kmer_file$datapath)

        } else if (input$sample != "Select sample") {
            validate(
                need(file.exists(input$sample), "Sample doesn't exist")
            )
            return(input$sample)
        }
    })

    # open file and save into data frame
    reactive_df <- reactive({
        file <- filename()
        df = read.table(file)
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
            if (is.null(input$show_hide_button)) {
                show = TRUE
            } else {
                show = FALSE
            }
            r = simple_count_kmer(df,
                input$min_kmer, input$max_kmer,
                show_error=show
            )
        }
        return(r)
    })
    
    peak_plot_data <- reactive({
        df <- reactive_df()
        if (is.null(input$min_kmer) || is.null(input$max_kmer)) {
            r = peak_count_kmer(df, show_error=FALSE)
        } else {
            if (is.null(input$show_hide_button)) {
                show = TRUE
            } else {
                show = FALSE
            }
            
            if (input$genome_type == "diploid") {
                num_peaks = 2
            } else {
                num_peaks = 1
            }
            r = peak_count_kmer(df,
                input$min_kmer, input$max_kmer,
                show_error = show, num_peaks = num_peaks
            )
        }
        return(r)
    })

    genome_scope_data = reactive({
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
        
        sliderInput("min_kmer", "Starting Frequency",
            min = 0, max = max_freq, value = val
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
        
        # make sure value is >= start_freq
        if (val < input$min_kmer) {
            val <- input$min_kmer
        }
        
        # create slider
        sliderInput("max_kmer", "Ending Frequency",
            min = input$min_kmer,
            max = max_freq,
            value = val
        )
    })

    # generate results
    output$simple_count_plot <- renderPlotly({
        r <- simple_plot_data()
        r$graph
    })

    output$simple_size <- renderText({
        r <- simple_plot_data()
        r$size
    })

    output$peak_freq_plot <- renderPlotly({
        r <- peak_plot_data()
        r$graph
    })

    output$freq_size <- renderText({
        r <- peak_plot_data()
        r$size
    })

    output$genome_scope_linear_plot <- renderPlotly({
        r = genome_scope_data()
        r$linear_plot
    })

    output$genome_scope_log_plot <- renderPlotly({
        r = genome_scope_data()
        r$log
    })

    output$genome_scope_summary <- renderTable(rownames = TRUE, {
        r = genome_scope_data()
        r$summary
    })

    output$genome_scope_size <- renderText({
        r = genome_scope_data()
        r$size
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
