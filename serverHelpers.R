# enable output tab
enable_output <- function() {
    show(selector = "#navbar li a[data-value=nav_output]")
    enable(selector = "#navbar li a[data-value=nav_output]")
}

# disable output tab
disable_output <- function() {
    hide(selector = "#navbar li a[data-value=nav_output]")
    disable(selector = "#navbar li a[data-value=nav_output]")
}

# Given with a list of widgets, enable or disable all the widgets depending on the given bool
toggle_widgets <- function(widgets, is_enable) {
    for(widget in widgets) {
        if (is_enable) {
            enable(widget)
        } else {
            disable(widget)
        }
    }
}

# Toggles heterozygosity widget
toggle_heterozygosity <- function(input) {
    if(input$sim_genome_type == "sim_diploid") {
        enable("sim_heterozygosity")
    } else {
        disable("sim_heterozygosity")
    }
}

# Returns a summary of input values for the output page
get_output_summary <- function(input, widgets) {
    summary_table <- renderTable(
        hover=TRUE, spacing = c("s"), {
        reactive({
            x <- reactiveValuesToList(input)[widgets]
            x$kmer_file = x$kmer_file$name
            
            labels = gsub("_", " ", names(x))
            labels = sub("^(\\w)(\\w*)", "\\U\\1\\L\\2", labels, perl=TRUE)
            
            # print(labels)
            
            data.frame(
                names = labels,
                values = unlist(x, use.names = FALSE)
            )
        })()
    })
    
    return (summary_table)
}