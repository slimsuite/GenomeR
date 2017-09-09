library(ggplot2)
library(plotly)

simple_count_kmer <- function(filename, start_freq = 2) {
    df = read.table(filename)
    names(df) = c("Frequency", "Count")
    
    if (start_freq < 1) {
        start_freq = 1
    }
    end_freq = max(df$Frequency)
    peak_freq = df[df$Count == max(df$Count[start_freq:end_freq]), "Frequency"]
    
    graph = ggplot(df[start_freq:end_freq,], aes(x = Frequency, y = Count)) + geom_line() + labs(title = "Count VS Frequency")
    size = sum(as.numeric(df[start_freq:end_freq, "Frequency"] * df[start_freq:end_freq, "Count"])) / peak_freq
    
    return (list("graph" = graph, "size" = size))
}

# setwd("~/unsw/binf3111/binf3111-genomer")
r <- simple_count_kmer("./inputk21.hist.txt")

