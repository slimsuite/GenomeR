library(shiny)
library(shinyjs)
source("simpleCountKmer.R")

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
    
    #
    # Object/Event listeners
    #
    
    # if switching to user input switch focus, disable simulation and enable input settings
    observeEvent(input$type, {
        if (input$type == "File input") {
            toggle_widgets(sim_widgets, FALSE)
            toggle_widgets(input_widgets, TRUE)
            output$summary <- get_output_summary(input, input_widgets)
        } else {
            toggle_widgets(sim_widgets, TRUE)
            toggle_widgets(input_widgets, FALSE)
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
    output$simple_plot <- renderPlot({
        # hist(rnorm(input$kmer_length))
        validate(
            need(input$kmer_file, 'Please upload a jellyfish kmer profile')
            # need(correct_format(input$kmer_file), 'another error')
        )
        
        r = simple_count_kmer(input$kmer_file$datapath, input$start_freq)
        output$simple_size <- renderText({r$size})
        r$graph
    })
})

# enable output tab
enable_output <- function() {
    show(selector = "#navbar li a[data-value=nav_output]")
    enable(selector = "#navbar li a[data-value=nav_output]")
}

# disable output tab
disable_output <- function() {
    hide(selector = "#navbar li a[data-value=nav_output]")
    disable(selector = "#navbar li a[data-value=nav_output]")
}

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
