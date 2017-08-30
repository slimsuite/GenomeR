library(shiny)
source("inputPage.R")
source("outputPage.R")

shinyUI(
  navbarPage('Genomer',
    tabPanel('Page 1', inputPage()),
    tabPanel('Page 2', outputPage())
  )
)