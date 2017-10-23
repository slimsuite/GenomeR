library(plotly)

simulate <- function(size = 5000000, coverage = 50, max_kmer = 100, error_rate = 4, diploid = FALSE, kmer_length = 21, read_length = 150, het_rate = 0.01, 
                     prob=0.7) {
    error_rate = error_rate / 100
    num_kmers = size * coverage - read_length*(kmer_length - 1)
    num_correct = size
    num_error = error_rate*coverage*size
    
    x = seq(1, max_kmer, 1)  # all frequencies
    dist = dpois(x, coverage)  # dist of perfect model - prob
    error = num_error * dgeom(x, prob=prob)  # dist of errors - count
    
    if (diploid) {
        het_size = het_rate * size * kmer_length
        hom_size = size - het_size / 2
        het_dist = dpois(x, coverage / 2) * het_size
        dist = dist * hom_size
        dist = dist + het_dist
        final = dist
    } else {
        final = size * dist
    }
    
    # move error kmers
    # i = 1
    # while (round(error[i]) > 0) {
    #     count = error[i]  # find number of errors of repeat i
    #     move = count * dist  # spread those errors evenly over dist
    #     move[final - move < 0] = final[final - move < 0]
    #     final = final - move  # remove the error kmers from count
    #     move = c(move[-(1:i)], rep(0, i))  # shift left them by i
    #     final = final + move  # add them back
    #     i = i + 1
    # }
    
    y = final + error
    return(data.frame(x, y))
}