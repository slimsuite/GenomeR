library(shinyjs)

inputPage <- function() {
    useShinyjs()

    fluidRow(
        column(
            width = 5,
            offset = 1,
            h3("Input Settings")
        ),
            
        column(
            width = 5,
            h3("Simulation Settings")
        )
    )
    
    # file/sim input toggle row
    fluidRow(
        column(
            width = 10,
            offset = 1,
            align = "center",
            radioGroupButtons(
                inputId = "user_input", label = NULL, 
                choices = c("File input", "Simulation input"), 
                justified = TRUE, status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            )
        )
    )
    
    # inputs row
    fluidRow(
        column(
            width = 5,
            offset = 1,
            fileInput("kmer_file", "K-mer Profile"),
            selectInput("sample", "Choose from sample k-mer profiles", c("sample 1", "sample 2")),
            numericInput("kmer_length", "K-mer length", 21),
            numericInput("read_length", "Read length", 100),
            numericInput("max_kmer_coverage", "Maximum k-mer coverage", 100)
        ),
        
        column(
            width = 5,
            numericInput("sim_genome_size", "Genome Size", 3000000000),
            radioButtons("sim_genome_type", "Haploid or Diploid", choiceNames = c("Haploid", "Diploid"), choiceValues = c("sim_haploid", "sim_diploid")),
            numericInput("sim_heterozygosity", "Heterozygosity (%)", 25)
        )
    )
    
    # submit button row
    fluidRow(
        column(
            width = 10,
            offset = 1,
            align = "center",
            actionButton("submit", "Submit")
        )
    )
}
