library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(plotly)
source("mainPage.R")
source("resultsPage.R")
source("batchPage.R")
source("Instruction.R")

shinyUI(
    tagList(
        useShinyjs(),
        navbarPage(title="GenomeR", id="navbar",
            tabPanel("Instruction",Instruction()),
            tabPanel("Model", value="nav_models", mainPage()),
            tabPanel("Results", value="nav_results", resultsPage()),
            tabPanel("Batch Analysis", value="nav_batch", batchPage()),
            tabPanel(title=HTML("</a></li><li><a href='https://bitbucket.org/violetbrina/binf3111-genomer/wiki/Home' target='_blank'>Help</a></li>"), value="nav_help")
        )
    )
)




