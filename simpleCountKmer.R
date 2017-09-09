#
# Estimate genome size by counting unique kmers
#
# filename -> name of .histo file
# start_freq -> frequency to start from with kmer counting
# end_freq -> frequency to end from with kmer counting
#             either +ve number > start_freq OR -ve number indicating how far from the end to stop counting
# highlighted -> TRUE : highlight discounted regions, FALSE : plot only counted region
simple_count_kmer <- function(filename, start_freq = 0, end_freq = NULL, highlighted = TRUE) {
    df = read.table(filename)
    names(df) = c("Frequency", "Count")
    
    # freq and count max values
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
    
    # print(c(start_freq, end_freq))
    
    # get peak of plot
    peak_freq = df[df$Count == max(df$Count[start_freq:end_freq]), "Frequency"]
    
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
        p = plot_ly(df[start_freq:end_freq,], x=~Frequency, y=~Count, type="scatter", mode="lines")
    }
    
    # plot with shapes
    p = layout(p, shapes = rectangles)
    
    # calculate size using simple unique kmer counting
    size = sum(as.numeric(df[start_freq:end_freq, "Frequency"] * df[start_freq:end_freq, "Count"])) / peak_freq
    
    return (list("graph" = p, "size" = size))
}

# Testing
# setwd("~/unsw/binf3111/binf3111-genomer")
# r <- simple_count_kmer("./inputk21.hist.txt", start_freq = 10, end_freq = -100, highlighted = TRUE)
# r$graph
 