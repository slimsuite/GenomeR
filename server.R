library(shiny)
library(shinyjs)

shinyServer(function(input, output, session) {
    # disable simulation by default
    disable("sim_genome_size")
    disable("sim_genome_type")
    disable("sim_heterozygosity")
    
    # if switching to simulation swap focus and disable file input
    observeEvent(input$simulation, {
        disable("kmer_file")
        disable("sample")
        disable("kmer_length")
        disable("read_length")
        disable("max_kmer_coverage")
        
        enable("sim_genome_size")
        enable("sim_genome_type")
        
        if(input$sim_genome_type == "sim_diploid") {
            enable("sim_heterozygosity")
        } else {
            disable("sim_heterozygosity")
        }
    })
    
    # if switching to user input switch focus and disable simulation
    observeEvent(input$user_input, {
        disable("sim_genome_size")
        disable("sim_genome_type")
        disable("sim_heterozygosity")
        
        enable("kmer_file")
        enable("sample")
        enable("kmer_length")
        enable("read_length")
        enable("max_kmer_coverage")
    })
    
    # listener to enable heterozygosity only for diploid genomes
    observe({
        if(input$sim_genome_type == "sim_diploid") {
            enable("sim_heterozygosity")
        } else {
            disable("sim_heterozygosity")
        }
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
