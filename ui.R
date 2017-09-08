library(shiny)
library(shinyjs)
library(shinyWidgets)
source("inputPage.R")
source("outputPage.R")

shinyUI(
    tagList(
        useShinyjs(),
        navbarPage("Genomer",
            id="navbar",
            tabPanel("Input", value="nav_input", inputPage()),
            tabPanel("Output", value="nav_output", outputPage())
        )
    )
)