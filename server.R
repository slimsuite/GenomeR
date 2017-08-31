library(shiny)
library(shinyjs)

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
    input_widgets = c("kmer_file", "sample", "kmer_length", "read_length", "max_kmer_coverage")
    sim_widgets = c("sim_genome_size", "sim_genome_type", "sim_heterozygosity")
    
    # disable simulation by default
    toggle_widgets(sim_widgets, FALSE)
    
    # if switching to simulation swap focus, disable input settings and enable simulation settings
    observeEvent(input$simulation, {
        toggle_widgets(input_widgets, FALSE)
        toggle_widgets(sim_widgets, TRUE)
        toggle_heterozygosity(input)
    })
    
    # if switching to user input switch focus, disable simulation and enable input settings
    observeEvent(input$user_input, {
        toggle_widgets(sim_widgets, FALSE)
        toggle_widgets(input_widgets, TRUE)
    })
    
    # listener to enable heterozygosity only for diploid genomes
    observe({
        toggle_heterozygosity(input)
    })
    
    # navigate to the results page on input submition
    # TODO input checking
    observeEvent(input$submit, {
        updateNavbarPage(session, "navigation", "results")
    })
    
    output$test <- renderText({
        input$kmer_length;
    });
})
