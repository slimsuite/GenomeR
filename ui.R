library(shiny)
library(shinyWidgets)
source("inputPage.R")
source("outputPage.R")

shinyUI(
    navbarPage('Genomer',
        id='navigation',
        tabPanel('Input', value='input', inputPage()),
        tabPanel('Output', value='output', outputPage())
    )
)