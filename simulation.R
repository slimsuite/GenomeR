simulate <- function(size = 5000000, coverage = 50, max_kmer = 300, error_rate = 0.01, diploid = FALSE) {
    num_correct = (1-error_rate)*size
    num_error = error_rate*size
    x = seq(1, max_kmer)
    y = num_correct * dpois(x, coverage)
    print(typeof(y))
    
    
    error = c(num_error, rep(0, max_kmer))
    print(error)
    print(typeof(error))
    # y = y + error
    plot(x, y)
    print(sum(y))
    
    return(data.frame(x, y))
}