#Introduction to GenomeR
Instruction <- function() { fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    mainPanel(
              h2("Introduction to GenomeR"),
              br(),
              ###
              h4("Function:"),
              p("GenomeR estimates the size of sequencing data using three different algorithms and allows users to interact with the resulted models and compare between the results of the algorithms"),
              br(),
              ###
              h4("Three algorithms:"),
              strong("Simple Counting Method"),p(" - Simply counts the number of k-mers"),   
              strong("Peak K-mer Identifying Method"),p(" - Identifies the peak of the k-mer distribution and estimates the size of the genome"),
              strong("GenomeScope"),p("- Estimates the overall characteristics of a genome"),
              br(),
              ###
              h4("Input data:"),
              p(
                'GenomeR takes in a histogram of k-mer frequencies as its input. You can use tools such as ',
                       a('Jellyfish',href = 'http://www.genome.umd.edu/jellyfish.html'),
                       "to compute the histogram. Below is an example command of running Jellyfish:"),
              code("jellyfish count -m 21 -s 100M -t 10 -C reads.fasta && jellyfish histo -t 10 reads.jf > reads.histo  ")
              ),
              br(),
              ###
              h4("Parameters:"),
              a('k-mer',href = 'https://en.wikipedia.org/wiki/K-mer'),
              br(),
              a('read length',href = 'https://en.wikipedia.org/wiki/Read_(biology)')
    
    

)}

