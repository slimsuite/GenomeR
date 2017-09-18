library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(plotly)
source("inputPage.R")
source("outputPage.R")
source("summaryPage.R")

shinyUI(
    tagList(
        useShinyjs(),
        navbarPage("Genomer",
            id="navbar",
            tabPanel("Input", value="nav_input", inputPage()),
            tabPanel("Output", value="nav_output", outputPage()),
            tabPanel("Summary", value="nav_summary", summaryPage())
        )
    )
)




