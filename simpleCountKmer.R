#
# Estimate genome size by counting unique kmers
#
# filename -> name of .histo file
# start_freq -> frequency to start from with kmer counting
# end_freq -> frequency to end from with kmer counting
#             either +ve number > start_freq OR -ve number indicating how far from the end to stop counting
# show_error -> TRUE : highlight discounted regions, FALSE : plot only counted region
simple_count_kmer <- function(df, start_freq = 0, end_freq = NULL, show_error = TRUE) {
    # df = read.table(filename)
    # names(df) = c("Frequency", "Count")
    
    # freq and count max values
    max_count = max(df$Count)
    max_freq = max(df$Frequency)
    
    # determine start and end
    if (is.null(start_freq)) {
        start_freq = calc_start_freq(df)
    } else if (start_freq < 0) {
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

    # ggplot version
    # graph = ggplot(df[start_freq:end_freq,], aes(x = Frequency, y = Count)) + geom_line()
    
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
    
    # plot with shapes
    p = plot_ly(plot_data, x= ~Frequency, y= ~Count, name = "Count", type="bar")
    p = layout(p, bargap = 0.01, bargroupgap = 0.01, shapes = rectangles)
    
    # calculate size using simple unique kmer counting
    size = sum(as.numeric(rows$Count))
    total_kmers = as.integer(sum(as.numeric(df$Frequency)))
    error = total_kmers - size
    
    return (list("graph" = p, "size" = size, "total_kmers" = total_kmers, "error" = error))
}

# Testing
# # setwd("~/unsw/binf3111/binf3111-genomer")
# start_freq = 7
# end_freq = 200
# df <- read.table("www/small.histo")
# names(df) <- c("Frequency", "Count")
# rownames(df) <- df$Frequency
# rows = df[df$Frequency >= start_freq & df$Frequency <= end_freq,]
# print(rows$Count)
# r <- simple_count_kmer(df, start_freq = 10, end_freq = -100, show_error = TRUE)
# r$graph

# 
# df2 <- data.frame(list(x=c(1:50), y=rnorm(50)))
# p <- plot_ly(df2) %>%
#      # add_trace(x = ~x, y = ~y, type = 'bar', width = 0.05) %>%
#      # add_trace(x = ~x, y = ~y, type = 'bar', width = 0.15) %>%
#      add_trace(x = ~x, y = ~y, type = 'bar', width = 0.5) %>%
#      layout(bargap = 1)
# p
