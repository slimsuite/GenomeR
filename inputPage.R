inputPage <- function() {

    col_width <- 5
    col_offset <- 1

    fixedPage(
        # forces width: 100% so all inputs strech to match page width
        tags$head(
            tags$style(HTML("
                .shiny-input-container:not(.shiny-input-container-inline) {
                    width: 100%;
                    max-width: 100%;
                }

                .dim {
                    opacity: 0.5;
                }
                ")
            )
        ),
        
        # input rows
        fixedRow(
            
            column(
                id = "input-col",
                width = col_width,
                offset = col_offset,
                h3("Input Settings"),
                fileInput("kmer_file", "K-mer profile"),
                numericInput("kmer_length", "K-mer length", 21),
                numericInput("read_length", "Read length", 100),
                numericInput("max_kmer_coverage", "Maximum k-mer coverage", 100)
            ),
            
            column(
                id = "sim-col",
                width = col_width,
                h3("Simulation Settings"),
                selectInput("sample", "Choose a sample k-mer profile", c("sample 1", "sample 2")),
                h5("OR", align="center"),
                numericInput("sim_genome_size", "Genome size", 3000000000),
                div(style = "height: 59px; margin-bottom: 15px;",
                    radioGroupButtons(inputId = "sim_genome_type", label = "Ploidy type",
                        choices = c("Haploid" = "sim_haploid", "Diploid" = "sim_diploid")
                    )
                ),
                numericInput("sim_heterozygosity", "Heterozygosity (%)", 25)
            )
        ),

        # file/sim input toggle row
        # see: https://dreamrs.github.io/shinyWidgets/
        fixedRow(
            column(
                width = col_width * 2,
                offset = col_offset,
                align = "center",
                radioGroupButtons(
                    inputId = "type", label = NULL, 
                    choices = c("File input", "Simulation input"), 
                    justified = TRUE, status = "primary",
                    checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                )
            )
        ),
        
        # submit button row
        fixedRow(
            br(),
            column(
                width = col_width * 2,
                offset = col_offset,
                align = "center",
                actionButton("submit", "Submit", class="btn-md btn-primary")
            )
        )
    )
}
