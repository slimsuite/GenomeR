library(shiny)
library(shinyjs)
library(shinyWidgets)
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
        df <- file_df()
        r = simple_count_kmer(df, input$freq_range[1], input$freq_range[2])
        output$simple_size <- renderText({r$size})
        r$graph
    })
    
    output$freq_slider <- renderUI({
        df <- file_df()
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
    
    # open file and save into data frame
    file_df <- reactive({
        validate(
            need(input$kmer_file, 'Please upload a jellyfish kmer profile')
            # need(correct_format(input$kmer_file), 'another error')
        )
        
        df = read.table(input$kmer_file$datapath)
        names(df) = c("Frequency", "Count")
        return(df)
    })
})
