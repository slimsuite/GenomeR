library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(plotly)
source("mainPage.R")
source("resultsPage.R")
source("batchPage.R")

shinyUI(
    tagList(
        useShinyjs(),
        navbarPage(
            "GenomeR",
            id="navbar",
            tabPanel("Model", value="nav_models", mainPage()),
            tabPanel("Results", value="nav_results", resultsPage()),
            tabPanel("Batch Analysis", value = "nav_batch", batchPage())
        )
    )
)




