library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(plotly)
source("mainPage.R")
source("summaryPage.R")

shinyUI(
    tagList(
        useShinyjs(),
        navbarPage("Genomer",
            id="navbar",
            tabPanel("Model", value="nav_output", mainPage()),
            tabPanel("Summary", value="nav_summary", summaryPage())
        )
    )
)




