library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(plotly)
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