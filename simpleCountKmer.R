simple_count_kmer <- function(filename, start_freq = 7) {
    df = read.table(filename)
    names(df) = c("Frequency", "Count")
    end_freq = length(df$Frequency)
    peak_freq = df[df$Count == max(df$Count[start_freq:end_freq]), "Frequency"]
    
    graph = ggplot(df[start_freq:end_freq,], aes(x = Frequency, y = Count)) + geom_line()
    size = sum(as.numeric(df[start_freq:end_freq, "Frequency"] * df[start_freq:end_freq, "Count"])) / peak_freq
    
    return (list("graph" = graph, "size" = size))
}

setwd("~/unsw/binf3111/binf3111-genomer")
r <- simple_count_kmer("inputk21.hist")
