# enable output tab
enable_output <- function() {
    select = "ul#navbar > li > a"
    enable(selector = select)
    shinyjs::show(selector = select)
}

# disable output tab
disable_output <- function() {
    select = "ul#navbar > li > a"
    disable(selector = select)
    hide(selector = select)
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

# Returns a summary of input values for the output page
get_output_summary <- function(input, widgets) {
    widget_descriptions = list("kmer_file" = "K-mer Profile", "kmer_length" = "K-mer Length",
            "read_length" = "Read Length", "max_kmer_coverage" = "Max K-mer Coverage", "sample" = "Sample",
            "sim_genome_size" = "Simulated Genome Size", "sim_genome_type" = "Simulated Genome Type",
            "sim_heterozygosity" = "Simulated Heterozygosity")

    summary_table <- renderTable(hover=TRUE, spacing = c("s"), {
        reactive({
            x <- reactiveValuesToList(input)[widgets]
            x$kmer_file = x$kmer_file$name
            labels = unlist(widget_descriptions[names(x)], use.names = FALSE)

            x$sample = if (exists("x$sample")) basename(x$sample) else NULL
            
            df <- data.frame(
                Inputs = labels,
                Values = unlist(x, use.names = FALSE)
            )
        })()
    })
    
    return (summary_table)
}

# show genomescope model settings
toggle_settings <- function(hide = NULL, show = NULL, anim = FALSE, anim_type = "slide") {
    for (h in hide) {
        shinyjs::hide(h, anim, anim_type)
    }
    for (s in show) {
        shinyjs::show(s, anim, anim_type)
    }
}