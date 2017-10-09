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
    
    all_settings = c("minkmer_slider", "maxkmer_slider", "genome_type", "show_hide_button", "gscope_type", "gscope_summary")
    genomescope_set = c("maxkmer_slider",  "gscope_type", "gscope_summary")
    simplecount_set = c("minkmer_slider", "maxkmer_slider", "show_hide_button")
    peakfreq_set = c("minkmer_slider", "maxkmer_slider", "genome_type", "show_hide_button")
    
    #
    # Initial conditions
    #
    
    # disable output by default
    # disable_output()
    
    # disable simulation by default
    toggle_widgets(toggle_sim_widgets, FALSE)
    output$input_summary <- get_output_summary(input, input_widgets)
    
    #
    # Object/Event listeners
    #
    
    # if switching to user input switch focus, disable simulation and enable input settings
    observeEvent(input$type, {
        if (input$type == "file") {
            toggle_widgets(input_widgets, TRUE)
            shinyjs::show("input-col")
            shinyjs::hide("sample-col")
            shinyjs::hide("sim-col")
            output$input_summary <- get_output_summary(input, input_widgets)
        } else if (input$type == "sample") {
            toggle_widgets(toggle_sim_widgets, TRUE)
            shinyjs::hide("input-col")
            shinyjs::show("sample-col")
            shinyjs::hide("sim-col")
            output$input_summary <- get_output_summary(input, all_sim_widgets)
        } else {
            toggle_widgets(toggle_sim_widgets, TRUE)
            shinyjs::hide("input-col")
            shinyjs::hide("sample-col")
            shinyjs::show("sim-col")
            output$input_summary <- get_output_summary(input, all_sim_widgets)
        }
    })
    
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

    # update freq slider based on max kmer coverage numeric input
    observe({
        updateNumericInput(session, "max_kmer_coverage", value = input$max_kmer)
    })

    observe({
        updateNumericInput(session, "max_kmer", value = input$max_kmer_coverage)
    })

    # navigate to the results page on input submition
    # TODO input checking
    observeEvent(input$submit, {
        disable_output()

        #checks file has been selected
        if (input$type == "file") {
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

            if (input$kmer_length > input$read_length) {
                showNotification("Kmer-length cannot be greater than read length", type="error")
                return(FALSE)
            }
        } else if (input$type == "sample") {
            return(TRUE)
        } else if (input$type == "simulation") {
            showNotification("Simulation currently unavailable", type="error")
            return(FALSE)
        }

        # checks passed, move to output page
        enable_output()
        updateNavbarPage(session, "navbar", "nav_output")
        return(TRUE)
    })

    observeEvent(input$plot_type, {
        if (input$plot_type == "gscope") {
            show_settings(hide = setdiff(all_settings, genomescope_set), show = genomescope_set)
        } else if (input$plot_type == "simple") {
            show_settings(hide = setdiff(all_settings, simplecount_set), show = simplecount_set)
        } else if (input$plot_type == "peak") {
            show_settings(hide = setdiff(all_settings, peakfreq_set), show = peakfreq_set)
        }
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
                show = FALSE
            } else if (input$show_hide_button == "hide_error") {
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
            if (is.null(input$show_hide_button)) {
                show = FALSE
            } else if (input$show_hide_button == "hide_error") {
                show = FALSE
            } else if (input$show_hide_button == "show_error") {
                show = TRUE
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
        df = reactive_df()

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
        rs <- simple_plot_data()
        rp <- peak_plot_data()
        rg <- gscope_data()

        outdf <- data.frame(Method=c("Simple Count", "Peak Frequency", "GenomeScope"),
                            Size=c(rs$size, rp$size, rg$size))

       # model_table <- outdf
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
