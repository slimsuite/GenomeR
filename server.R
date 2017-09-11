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
    observeEvent(input$submit, {
        
        #checks file has been selected
        if (input$type == "File input" && is.null(input$kmer_file)) {
           showNotification("Please upload a kmer profile", type="error")
        
        #check file is not empty
        } else if (file.size(input$kmer_file$datapath) != 0) {
            data <- read.table(input$kmer_file$datapath)
            
            #check correct number of columns
            if(ncol(data) != 2){
                showNotification("Wrong number of columns in input", type="error")
            
            } else {
                enable_output()
                updateNavbarPage(session, "navbar", "nav_output")
            }
        
        } else {
            showNotification("Input file is empty", type="error")
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
        
        r = simple_count_kmer(input$kmer_file$datapath, input$freq_range[1], input$freq_range[2])
        output$simple_size <- renderText({r$size})
        r$graph
    })
    
    output$freq_slider <- renderUI({
        df <- file_df()
        max_freq <- max(df$Frequency)
        
        print(input$freq_range)
        
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
        df = read.table(input$kmer_file$datapath)
        names(df) = c("Frequency", "Count")
        return(df)
    })
})
