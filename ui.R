library(shiny)
source("inputPage.R")
source("outputPage.R")

shinyUI(
    navbarPage('Genomer',
        id='navigation',
        tabPanel('Input', value='input', inputPage()),
        tabPanel('Results', value='results', outputPage())
    )
)