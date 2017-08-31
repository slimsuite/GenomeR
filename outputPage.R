outputPage <- function() {
    titlePanel("Genomer")
    sidebarLayout(
        sidebarPanel(
            "Output Summary",
            p("content"),
            textOutput("kmer_length")
        ),
    
        mainPanel(
            h1("This is Genomer WOHO!"),
            p("I'm so happy!"),
            p("I love you all <3"),
            p("Please keep adding features to me :D"),
            p("I promise I won't throw too many errors :P")
        )
    )
}