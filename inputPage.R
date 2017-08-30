library(shinyjs)

inputPage <- function() {
    fluidRow(
        useShinyjs(),
        column(
            width = 12,
            fluidRow(
                column(
                    width = 5,
                    actionLink("user_input", h3("Input Settings")),
                    fileInput("kmer_file", "K-mer Profile"),
                    selectInput("sample", "Choose from sample k-mer profiles", c("sample 1", "sample 2")),
                    numericInput("kmer_length", "K-mer length", 21),
                    numericInput("read_length", "Read length", 100),
                    numericInput("max_kmer_coverage", "Maximum k-mer coverage", 100)
                ),
                
                column(
                    width = 2
                ),
                
                column(
                    width = 5, 
                    actionLink("simulation", h3("Simulation Settings")),
                    numericInput("sim_settings", "Sim number", 100)
                )
            )
        ),
        
        column(
            width = 2,
            offset = 5,
            actionButton("submit", "Submit")
        )
    )
}
