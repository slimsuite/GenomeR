#
# Estimate genome size by counting unique kmers
#
# df -> data.frame with Frequency and Count columns
# filename -> name of .histo file
# start_freq -> frequency to start from with kmer counting
# end_freq -> frequency to end from with kmer counting
#             either +ve number > start_freq OR -ve number indicating how far from the end to stop counting
# highlighted -> TRUE : highlight discounted regions, FALSE : plot only counted region
peak_count_kmer <- function(df, start_freq = 0, end_freq = NULL, highlighted = FALSE) {
    
    # initial max_freq
    max_count = max(df$Count)
    max_freq = max(df$Frequency)
    
    # determine start and end
    if (start_freq < 0) {
        start_freq = 0
    }
    
    if (is.null(end_freq) || end_freq > max_freq) {
        end_freq = max_freq
    }
    
    if (end_freq < 0) {
        end_freq = max_freq + end_freq
    }
    
    # get rows within freq range
    rows = df[df$Frequency >= start_freq & df$Frequency <= end_freq,]
    
    # freq and count max values recalculated using cutoffs
    if (!highlighted) {
        max_count = max(rows$Count)
        max_freq = max(rows$Count)
    }
    
    # get peak of plot
    max = max(rows$Count)
    peak_freq = min(rows[rows$Count == max, "Frequency"])
    
    # peak line
    line = list(
        type = "line",
        line = list(color = "grey", dash = "dash"),
        xref = "Frequency",
        yref = "Count"
    )
    line$x0 = peak_freq
    line$x1 = peak_freq
    line$y0 = 0
    line$y1 = max_count
    
    # ggplot version
    # graph = ggplot(df[start_freq:end_freq,], aes(x = Frequency, y = Count)) + geom_line()
    
    # plotly version
    # plot rectangles over ignored regions
    rectangles = NULL
    if (highlighted) {
        p = plot_ly(df, x=~Frequency, y=~Count, type="scatter", mode="lines")
        rectangles = list(
            # error rectangle low frequency end
            list(type = "rect",
                 fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                 x0 = 0, x1 = start_freq, xref = "Frequency",
                 y0 = 0, y1 = max_count, yref = "Count"
            ),
            
            # error rectangle high frequency end
            list(type = "rect",
                 fillcolor = "red", line = list(color = "red"), opacity = 0.3,
                 x0 = end_freq, x1 = max_freq, xref = "Frequency",
                 y0 = 0, y1 = max_count, yref = "Count"
            )
        )
    } else {
        # plot only counted region
        p = plot_ly(rows, x=~Frequency, y=~Count, type="scatter", mode="lines")
    }
    
    # plot with shapes
    p = layout(p, shapes = append(rectangles, list(line)))
    p$elementId <- NULL  #TODO temp approach to suppress warning
    
    # calculate size using simple unique kmer counting
    size = as.integer(sum(as.numeric(rows$Frequency * rows$Count)) / peak_freq)
    
    return (list("graph" = p, "size" = size))
}

# Testing
# df = read.table("sharky.histo")
# names(df) = c("Frequency", "Count")
# r <- peak_count_kmer(df, start_freq = 5, end_freq = -5, highlighted = FALSE)
# r$graph


