library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(plotly)
source("inputPage.R")
source("simulationPage.R")
source("summaryPage.R")

shinyUI(
    tagList(
        useShinyjs(),
        navbarPage("Genomer",
            id="navbar",
            tabPanel("Input", value="input", inputPage()),
            tabPanel("Simulation", value="simulation", simulationPage()),
            tabPanel("Summary", value="summary", summaryPage())
        )
    )
)




