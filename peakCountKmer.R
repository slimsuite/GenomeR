#
# Estimate genome size by counting unique kmers
#
# df -> data.frame with Frequency and Count columns
# filename -> name of .histo file
# start_freq -> frequency to start from with kmer counting
# end_freq -> frequency to end from with kmer counting
#             either +ve number > start_freq OR -ve number indicating how far from the end to stop counting
# highlighted -> TRUE : highlight discounted regions, FALSE : plot only counted region
peak_count_kmer <- function(df, start_freq = 0, end_freq = NULL, highlighted = TRUE, num_peaks = 1) {
    library(quantmod)
    
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
    
    # plotly version
    # plot rectangles over ignored regions
    plot_data = rows       # plot only counted region
    rectangles = NULL
    if (highlighted) {
        plot_data = df
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
    }
    
    # get peak_rows
    # print(rows$Count)
    peak_rows = findPeaks(rows$Count)-1
    peak_rows = peak_rows[1:num_peaks]
    # print(peak_rows)
    
    # traces
    Frequency = plot_data$Frequency
    Count = plot_data$Count
    Peak_freq = plot_data$Frequency[peak_rows]
    Peaks = plot_data$Count[peak_rows]
    
    # get peak of plot
    max = max(rows$Count)
    peak_freq = Peak_freq[1]
    # print(peak_freq)
    
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
    
    # create plot
    plot_data = data.frame(Frequency, Peak_freq, Count, Peaks)
    p = plot_ly(plot_data, x= ~Frequency, y= ~Count, name = "Count", type="scatter", mode="lines")
    p = add_trace(p, x= ~Peak_freq, y = ~Peaks, name = "Peaks", mode = "markers")
    p = layout(p, showlegend = FALSE, shapes = append(rectangles, list(line)))
    p$elementId <- NULL  #TODO temp approach to suppress warning
    
    # calculate size using simple unique kmer counting
    size = as.integer(sum(as.numeric(rows$Frequency * rows$Count)) / peak_freq)
    
    return (list("graph" = p, "size" = size))
}

# Testing
df = read.table("small.histo")
names(df) = c("Frequency", "Count")
r <- peak_count_kmer(df, start_freq = 6, end_freq = -200, highlighted = FALSE, num_peaks = 3)
r$graph


