library(shiny)
library(shinyjs)

shinyServer(function(input, output) {
    disable("sim_settings")
    observeEvent(input$simulation, {
        disable("kmer_file")
        disable("sample")
        disable("kmer_length")
        disable("read_length")
        disable("max_kmer_coverage")
        enable("sim_settings")
    })
    
    observeEvent(input$user_input, {
        disable("sim_settings")
        enable("kmer_file")
        enable("sample")
        enable("kmer_length")
        enable("read_length")
        enable("max_kmer_coverage")
    })
})
