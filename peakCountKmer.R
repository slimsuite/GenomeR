#
# Estimate genome size by counting unique kmers
#
# df -> data.frame with Frequency and Count columns
# filename -> name of .histo file
# start_freq -> frequency to start from with kmer counting, will autoset if start_freq = NULL
# end_freq -> frequency to end from with kmer counting
#             either +ve number > start_freq OR -ve number indicating how far from the end to stop counting
# show_error -> TRUE : highlight discounted regions, FALSE : plot only counted region
peak_count_kmer <- function(df, start_freq = NULL, end_freq = NULL, show_error = TRUE, num_peaks = 1) {
    library(quantmod)
    
    # initial max_freq
    max_count = max(df$Count)
    max_freq = max(df$Frequency)
    
    # determine start and end
    if (is.null(start_freq)) {
        start_freq = calc_start_freq(df)
    } else if (start_freq < 1) {
        start_freq = 1
    }
    
    if (is.null(end_freq) || end_freq > max_freq) {
        end_freq = max_freq
    } else if (end_freq < 0) {
        end_freq = max_freq + end_freq
    }
    
    # get rows within freq range
    rows = df[df$Frequency >= start_freq & df$Frequency <= end_freq,]
    
    # freq and count max values recalculated using cutoffs
    if (!show_error) {
        max_count = max(rows$Count)
        max_freq = max(rows$Count)
    }
    
    # plotly version
    # plot rectangles over ignored regions
    plot_data = rows       # plot only counted region
    rectangles = NULL
    if (show_error) {
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
    peak_rows = findPeaks(plot_data$Count)-1
    
    # print(peak_rows)
    # print(length(peak_rows))
    # print(num_peaks)
    if (num_peaks > length(peak_rows)) {
        num_peaks = length(peak_rows)
    }
    peak_rows = peak_rows[1:num_peaks]
    # print(peak_rows)
    
    # traces
    Peaks = plot_data[peak_rows,]        # get peak Freq and Count
    Peaks = Peaks[order(-Peaks$Count),]  # order on Count
    # print(Peaks)
    
    # get peak of plot
    peak_freq = Peaks$Frequency[num_peaks]

    # peak lines
    # initiate a line shape object
    line <- list(
        type = "line",
        line = list(color = "orange", dash = "dash"),
        xref = "Frequency",
        yref = "Count"
    )
    
    lines <- list()
    for (i in rownames(Peaks)) {
        peak = Peaks[i,]
        line[c("x0", "x1")] <- peak$Frequency
        line[["y0"]] <- 0
        line[["y1"]] <- max_count
        lines <- c(lines, list(line))
    }
    
    # combine all shapes
    shapes = append(rectangles, lines)
    
    # create plot
    Frequency = plot_data$Frequency
    Count = plot_data$Count
    p = plot_ly(plot_data, x= ~Frequency, y= ~Count,
                name = "Count", type="scatter", mode="lines")
    p = add_trace(p, x= ~Peaks$Frequency, y = ~Peaks$Count,
                  name = "Peaks", mode = "markers")
    p = layout(p, title = "Count VS Frequency", showlegend = FALSE, shapes = shapes)
    p$elementId = NULL  #TODO temp approach to suppress warning
    
    # calculate size using simple unique kmer counting
    # only use non-error rows
    size = as.integer(sum(as.numeric(rows$Frequency * rows$Count)) / peak_freq)
    if (num_peaks == 2) {
        if (Peaks$Count[2] < Peaks$Count[1]*0.1) {
            size = NA
        }
    }
    total_kmers = as.integer(sum(as.numeric(df$Frequency)))
    error = total_kmers - size
    
    return (list("graph" = p, "size" = size, "total_kmers" = total_kmers, "error" = error))
}

calc_start_freq <- function(df) {
    library(quantmod)
    # if start_freq not set we set the error
    valley_rows = findValleys(df$Count)-1
    start_freq = df$Frequency[valley_rows[1]]
    
    if (is.na(start_freq)) {
        start_freq = 0
    }
    
    return(start_freq)
}

# Testing
# df = read.table("small.histo")
# names(df) = c("Frequency", "Count")
# r <- peak_count_kmer(df, start_freq = NULL, end_freq = 100, show_error = FALSE, num_peaks = 1)
# r$graph
