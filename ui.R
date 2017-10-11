library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(plotly)
source("mainPage.R")
source("resultsPage.R")

shinyUI(
    tagList(
        useShinyjs(),
        navbarPage("Genomer",
            id="navbar",
            tabPanel("Model", value="nav_models", mainPage()),
            tabPanel("Results", value="nav_results", resultsPage())
        )
    )
)




