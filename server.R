library(shiny)
library(shinyjs)

shinyServer(function(input, output) {
    disable("sim_genome_size")
    disable("sim_genome_type")
    disable("sim_heterozygosity")
    
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
    
    
    observe({
        if(input$sim_genome_type == "sim_diploid") {
            enable("sim_heterozygosity")
        } else {
            disable("sim_heterozygosity")
        }
    })
})
