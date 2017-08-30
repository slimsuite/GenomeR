library(shiny)
source("inputPage.R")
source("outputPage.R")

shinyUI(
  navbarPage('Genomer',
    tabPanel('Input', inputPage()),
    tabPanel('Results', outputPage())
  )
)