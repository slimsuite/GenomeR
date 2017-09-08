outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            h3("Input Summary"),
            tableOutput("summary"),
            sliderInput("start_freq", "Starting Frequency", min = 1, max = 100, value = 2)
            # actionLink("user_input", h3("Input Settings")),
            # fileInput("kmer_file", "K-mer Profile"),
            # selectInput("sample", "Choose from sample k-mer profiles", c("sample 1", "sample 2")),
            # numericInput("kmer_length", "K-mer length", 21),
            # numericInput("read_length", "Read length", 100),
            # numericInput("max_kmer_coverage", "Maximum k-mer coverage", 100)
        ),
    
        mainPanel(
            h3("Output Model"),
            plotOutput("simple_plot"),
            h3("Estimated Size"),
            textOutput("simple_size")
        )
    )
}