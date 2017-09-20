inputPage <- function() {
    
    col_width <- 5
    col_offset <- 1
    
    fixedPage(
        # forces width: 100% so all inputs strech to match page width
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        
        # input rows
        fixedRow(
            
            column(
                id = "input-col",
                width = col_width,
                offset = col_offset,
                fileInput("kmer_file", "K-mer profile"),
                numericInput("kmer_length", "K-mer length", 21),
                numericInput("read_length", "Read length", 100),
                numericInput("max_kmer_coverage", "Maximum k-mer coverage", 100)
            ),
            
            column(
                id = "sim-col",
                width = col_width,
                h3("Simulation Settings"),
                div(style = "height: 64px !important;",
                    selectInput("sample", "Choose a sample k-mer profile",
                        c("Select sample", "small.histo", "sharky.histo")
                    )  
                ),
                # br(style = "height: 40px;"),
                h5("OR", align="center"),
                numericInput("sim_genome_size", "Genome size", 3000000000),
                radioGroupButtons(inputId = "sim_genome_type", label = "Ploidy type",
                    choices = c("Haploid" = "sim_haploid", "Diploid" = "sim_diploid")
                ),
                numericInput("sim_heterozygosity", "Heterozygosity (%)", 25)
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
