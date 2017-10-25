library(plotly)

simulate <- function(size = 5000000, coverage = 50, max_kmer = 100, error_rate = 4, diploid = FALSE, kmer_length = 21, read_length = 150, het_rate = 0.01, prob=0.7) {
    
    # coverage = number_of_reads * read_length / size
    # number_of_reads = coverage * size / read_length
    number_of_reads = coverage * size / read_length
    
    # number of total kmers - accounting for the kmers lost at the end of each read
    num_kmers = size * coverage - number_of_reads*(kmer_length - 1)
    num_uniqe_kmers = num_kmers / coverage
    corrected_coverage = coverage * (read_length - kmer_length + 1)/read_length
    
    x = seq(1, max_kmer, 1)  # all frequencies
    dist = dpois(x, corrected_coverage)  # dist of perfect model - prob
    
    if (diploid) {
        het_size = het_rate * size * kmer_length
        hom_size = size - het_size / 2
        het_dist = dpois(x, corrected_coverage / 2) * het_size
        dist = dist * hom_size
        dist = dist + het_dist
        final = dist
    } else {
        final = num_uniqe_kmers * dist
    }
    
    # generate errors
    bp_error_rate = error_rate / 100  # from percentage to fraction
    error_kmers = kmer_length * bp_error_rate * size
    error = error_kmers * dgeom(x, prob=prob)  # dist of errors - count
    
    if (error) {
        y = final + error
    } else {
        y = final
    }
    
    return(data.frame(x, y))
}