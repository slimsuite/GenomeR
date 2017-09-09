library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
source("simpleCountKmer.R")     # functions to estimate genome size
source("serverHelpers.R")       # helper functions used in server.R

shinyServer(function(input, output, session) {
    
    #
    # Setup variables and any intermediary/conductor function
    #
    
    input_widgets = c("kmer_file", "kmer_length", "read_length", "max_kmer_coverage")
    sim_widgets = c("sample", "sim_genome_size", "sim_genome_type", "sim_heterozygosity")
    
    
    
    #
    # Initial conditions
    #
    
    # disable output by default
    disable_output()
    
    # disable simulation by default
    toggle_widgets(sim_widgets, FALSE)
    output$summary <- get_output_summary(input, input_widgets)
    
    # disable type - only allow user input for now
    # disable("type")
    
    
    
    #
    # Object/Event listeners
    #
    
    # if switching to user input switch focus, disable simulation and enable input settings
    observeEvent(input$type, {
        if (input$type == "File input") {
            toggle_widgets(sim_widgets, FALSE)
            toggle_widgets(input_widgets, TRUE)
            removeClass("input-col", "dim")
            addClass("sim-col", "dim")
            output$summary <- get_output_summary(input, input_widgets)
        } else {
            toggle_widgets(sim_widgets, TRUE)
            toggle_widgets(input_widgets, FALSE)
            addClass("input-col", "dim")
            removeClass("sim-col", "dim")
            output$summary <- get_output_summary(input, sim_widgets)
        }
    })
    
    # listener to enable heterozygosity only for diploid genomes
    observe({
        toggle_heterozygosity(input)
    })
    
    # navigate to the results page on input submition
    # TODO input checking
    observeEvent(input$submit, {
        if (input$type == "File input" && is.null(input$kmer_file)) {
            showNotification("Please upload a kmer profile", type="error")
        } else {
            enable_output()
            updateNavbarPage(session, "navbar", "nav_output")
        }
    })
    
    
    
    #
    # Generate outputs
    #
    
    # generate results
    output$simple_plot <- renderPlotly({
        # hist(rnorm(input$kmer_length))
        validate(
            need(input$kmer_file, 'Please upload a jellyfish kmer profile')
            # need(correct_format(input$kmer_file), 'another error')
        )
        
        r = simple_count_kmer(input$kmer_file$datapath, input$start_freq, input$end_freq)
        output$simple_size <- renderText({r$size})
        r$graph
    })
    
    output$start_freq_slider <- renderUI({
        df <- file_df()
        max_freq <- max(df$Frequency)
        val <- input$start_freq
        
        # set initial value
        if (is.null(val)) {
            val <- 0
        }
        
        sliderInput("start_freq", "Starting Frequency",
            min = 0,
            max = max_freq,
            value = val
        )
    })
    
    output$end_freq_slider <- renderUI({
        df <- file_df()
        max_freq <- max(df$Frequency)
        val <- input$end_freq
            
        # set initial val to max, otherwise keep current value
        if (is.null(val)) {
            val <- max_freq
        }
        
        # make sure value is >= start_freq
        if (val < input$start_freq) {
            val <- input$start_freq
        }
        
        # create slider
        sliderInput("end_freq", "Ending Frequency",
            min = input$start_freq,
            max = max_freq,
            value = val
        )
    })
    
    # open file and save into data frame
    file_df <- reactive({
        df = read.table(input$kmer_file$datapath)
        names(df) = c("Frequency", "Count")
        return(df)
    })
})
