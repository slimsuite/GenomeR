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
    output$summary <- get_output_summary(input, input_widgets)
    
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
            output$summary <- get_output_summary(input, input_widgets)
        } else {
            toggle_widgets(toggle_sim_widgets, TRUE)
            toggle_widgets(input_widgets, FALSE)
            addClass("input-col", "dim")
            removeClass("sim-col", "dim")
            output$summary <- get_output_summary(input, all_sim_widgets)
        }
    })
    
    # listener to enable heterozygosity only for diploid genomes
    observe({
        toggle_heterozygosity(input)
    })
    
    # navigate to the results page on input submition
    # TODO input checking
    observeEvent(input$submit, {

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
        return(df)
    })

    # generate plots and size estimates
    simple_plot_data <- reactive({
        highlight <- input$show_hide_button == "Show all"
        df <- reactive_df()
        if (is.null(input$freq_range)) {
            r = simple_count_kmer(df, highlighted=FALSE)
        } else {
            r = simple_count_kmer(df,
                input$freq_range[1], input$freq_range[2],
                highlighted=highlight
            )
        }
        return(r)
    })
    
    peak_plot_data <- reactive({
        highlight <- input$show_hide_button == "Show all"
        df <- reactive_df()
        if (is.null(input$freq_range)) {
            r = peak_count_kmer(df, highlighted=FALSE)
        } else {
            r = peak_count_kmer(df,
                input$freq_range[1], input$freq_range[2],
                highlighted=highlight
            )
        }
        return(r)
    })

    
    #
    # Generate outputs
    #

    # reactive ui elements
    output$freq_slider <- renderUI({
        df <- reactive_df()
        max_freq <- max(df$Frequency)
        
        # print(input$freq_range)
        
        start <- input$freq_range[1]
        end <- input$freq_range[2]
        
        # set initial value
        if (is.null(start)) {
            start <- 0
        }
        
        # set initial val to max, otherwise keep current value
        if (is.null(end)) {
            end <- max_freq
        }
        
        # create slider
        sliderInput("freq_range", "Valid Range",
            min = 0,
            max = max_freq,
            value = c(start, end)
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

    output$genome_scope_plot_1 <- renderPlot({
        file <- filename()
        validate(
            need(input$kmer_length, "Need kmer length"),
            need(input$max_kmer_coverage, "Need max_kmer_coverage")
        )
        r = runGenomeScope(file,
            input$kmer_length, input$read_length, "tmp",
            input$max_kmer_coverage
        )
        # output$simple_size <- renderText({r$size})
        r
    })

    output$genome_scope_plot_2 <- renderPlot({
        file <- filename()
        validate(
            need(input$kmer_length, "Need kmer length"),
            need(input$max_kmer_coverage, "Need max_kmer_coverage")
        )
        # PUT SECOND PLOT HERE
    })
})
