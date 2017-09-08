library(shiny)
library(shinyjs)
# source("countUniqueKmer.R")

# Given with a list of widgets, enable or disable all the widgets depending on the given bool
toggle_widgets <- function(widgets, is_enable) {
    for(widget in widgets) {
        if (is_enable) {
            enable(widget)
        } else {
            disable(widget)
        }
    }
}

# Toggles heterozygosity widget
toggle_heterozygosity <- function(input) {
    if(input$sim_genome_type == "sim_diploid") {
        enable("sim_heterozygosity")
    } else {
        disable("sim_heterozygosity")
    }
}

shinyServer(function(input, output, session) {
    
    #
    # Setup variables and any intermediary/conductor function
    #
    
    input_widgets = c("kmer_file", "sample", "kmer_length", "read_length", "max_kmer_coverage")
    sim_widgets = c("sim_genome_size", "sim_genome_type", "sim_heterozygosity")
    
    #
    # Initial conditions
    #
    
    # disable simulation by default
    toggle_widgets(sim_widgets, FALSE)
    
    #
    # Object/Event listeners
    #
    
    # if switching to user input switch focus, disable simulation and enable input settings
    observeEvent(input$user_input, {
        if (input$user_input == "File input") {
            toggle_widgets(sim_widgets, FALSE)
            toggle_widgets(input_widgets, TRUE)
        } else {
            toggle_widgets(sim_widgets, TRUE)
            toggle_widgets(input_widgets, FALSE)
        }
    })
    
    # listener to enable heterozygosity only for diploid genomes
    observe({
        toggle_heterozygosity(input)
    })
    
    # navigate to the results page on input submition
    # TODO input checking
    observeEvent(input$submit, {
        if (is.null(input$kmer_file)) {
            showNotification("Please upload a kmer profile", type="error")
        } else {
            updateNavbarPage(session, "navigation", "output")
        }
    })
    
    #
    # Generate outputs
    #
    
    # generate results
    output$test_plot <- renderPlot({
        # hist(rnorm(input$kmer_length))
        validate(
            need(input$kmer_file, 'Please upload a jellyfish kmer profile')
            # need(correct_format(input$kmer_file), 'another error')
        )
        
        file <- input$kmer_file
        data <- read.csv(file$datapath, sep=" ", header=FALSE)
        hist(rep(data$V1, data$V2))
    })
    
    # https://stackoverflow.com/questions/41031584/collect-all-user-inputs-throughout-the-shiny-app
    inputParams <- reactive({
        # print(user_input)
        if (file_input) {
            vals <- reactiveValuesToList(input)[input_widgets]
            labels <- input_widgets
        } else {
            vals <- reactiveValuesToList(input)[sim_widgets]
            labels <- sim_widgets
        }

        print(labels)
    })
    
    output$summary <- renderTable({
        inputParams()
    })
})
