############################################################################################################################################
#																																		   #
#						                            START OF CODE																		   #
#																																		   #
############################################################################################################################################


install_packages <- function(){
    
    .packages = c(
        "shinycssloaders",
        "shiny", 
        "shinyjs",
        "shinyWidgets", 
        "plotly",
        "ggplot2",
        "tools",
        "knitr",
        "shinyBS",
        "tidyverse"
    )
    
    # Install CRAN packages (if not already installed)
    .inst <- .packages %in% installed.packages()
    if(length(.packages[!.inst]) > 0) {install.packages(.packages[!.inst])}

    
    message("Packages installation:
            If there was no error then you are ready to do data analysis")
    
    
}

install_packages()

# Copy until here previous line!

####################################################END OF CODE###########################################################################

