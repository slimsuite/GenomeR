library(shiny)
library(shinyjs)
# source("countUniqueKmer.R")

shinyServer(function(input, output, session) {
    input_widgets = c("kmer_file", "sample", "kmer_length", "read_length", "max_kmer_coverage")
    sim_widgets = c("sim_genome_size", "sim_genome_type", "sim_heterozygosity")
    
    # disable simulation by default
    toggle_widgets(sim_widgets, FALSE)
    output$summary <- get_output_summary(input, input_widgets)
    
    # if switching to simulation swap focus, disable input settings and enable simulation settings
    observeEvent(input$simulation, {
        toggle_widgets(input_widgets, FALSE)
        toggle_widgets(sim_widgets, TRUE)
        toggle_heterozygosity(input)
        output$summary <- get_output_summary(input, sim_widgets)
    })
    
    # if switching to user input switch focus, disable simulation and enable input settings
    observeEvent(input$user_input, {
        toggle_widgets(sim_widgets, FALSE)
        toggle_widgets(input_widgets, TRUE)
        output$summary <- get_output_summary(input, input_widgets)
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
})

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

# Returns a summary of input values for the output page
get_output_summary <- function(input, widgets) {
    summary_table <- renderTable({
        reactive({
            x <- reactiveValuesToList(input)[widgets]
            x$kmer_file = x$kmer_file$name
            data.frame(
                names = names(x),
                values = unlist(x, use.names = FALSE)
            )
        })()
    })
    
    return (summary_table)
}
