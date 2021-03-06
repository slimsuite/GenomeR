#!/usr/bin/env Rscript

## GenomeScope: Fast Genome Analysis from Unassembled Short Reads
##
## This is the automated script for computing genome characteristics
## from a kmer histogram file, k-mer size, and readlength

# library(tidyverse)
library(tibble)
library(plotly)

## Number of rounds before giving up
# NUM_ROUNDS=4

## Coverage steps to trim off between rounds
# START_SHIFT=5

## Typical cutoff for sequencing error
# TYPICAL_ERROR = 15

## Max rounds on NLS
# MAX_ITERATIONS=20

## Overrule if two scores are within this percent (0.05 = 5%) but larger difference in het
# SCORE_CLOSE = 0.20

## Overrule heterozygosity if there is a large difference in het rate
# SCORE_HET_FOLD_DIFFERENCE = 10

## Print out VERBOSEging messages (0/1)
VERBOSE = 0

## Suppress the warnings if the modeling goes crazy, those are in try/catch blocks anyways
options(warn=-1)

## Colors for plots
COLOR_BGCOLOR  = "light grey"
COLOR_HIST     = "#56B4E9"
COLOR_4PEAK    = "black"
COLOR_2PEAK    = "#F0E442"
COLOR_ERRORS   = "#D55E00"
COLOR_KMERPEAK = "black"
COLOR_RESIDUAL = "purple"
COLOR_COVTHRES = "red"

## Given mean +/- stderr, report min and max value within 2 SE
###############################################################################

min_max <- function(table){
	##return (c( abs(table[1]) - 2*abs(table[2]) , abs(table[1])+ 2*abs(table[2])))
	return (c(table[1] - 2*table[2], table[1]+ 2*table[2]))
}



## Use nls to fit 4 peak model
###############################################################################

nls_4peak<-function(x, y, k, estKmercov, estLength, max_iterations){
	model4 = NULL

    if (VERBOSE) { cat("trying nls_4peak standard algorithm\n") }

	try(model4 <- nls(y ~ (((2*(1-d)*(1-(1-r)^k)) + (2*d*(1-(1-r)^k)^2) + (2*d*((1-r)^k)*(1-(1-r)^k))) * dnbinom(x, size = kmercov   / bias, mu = kmercov)     * length +
                          (((1-d)*((1-r)^k)) + (d*(1-(1-r)^k)^2))                                      * dnbinom(x, size = kmercov*2 / bias, mu = kmercov * 2) * length +
                          (2*d*((1-r)^k)*(1-(1-r)^k))                                                  * dnbinom(x, size = kmercov*3 / bias, mu = kmercov * 3) * length +
                          (d*(1-r)^(2*k))                                                              * dnbinom(x, size = kmercov*4 / bias, mu = kmercov * 4) * length),
                      start = list(d=0, r=0, kmercov=estKmercov, bias = 0.5, length=estLength),
                      control = list(minFactor=1e-12, maxiter=max_iterations)), silent = TRUE)

	if(class(model4) == "try-error"){
        if (VERBOSE) { cat("retrying nls_4peak with port algorithm\n") }

        try(model4 <- nls(y ~ (((2*(1-d)*(1-(1-r)^k)) + (2*d*(1-(1-r)^k)^2) + (2*d*((1-r)^k)*(1-(1-r)^k))) * dnbinom(x, size = kmercov   / bias, mu = kmercov)     * length +
                              (((1-d)*((1-r)^k)) + (d*(1-(1-r)^k)^2))                                      * dnbinom(x, size = kmercov*2 / bias, mu = kmercov * 2) * length +
                              (2*d*((1-r)^k)*(1-(1-r)^k))                                                  * dnbinom(x, size = kmercov*3 / bias, mu = kmercov * 3) * length +
                              (d*(1-r)^(2*k))                                                              * dnbinom(x, size = kmercov*4 / bias, mu = kmercov * 4) * length),
                          start = list(d=0, r=0, kmercov=estKmercov, bias = 0.5, length=estLength),
                          algorithm="port", control = list(minFactor=1e-12, maxiter=max_iterations)), silent = TRUE)
	}

	return(model4)
}


## score model by number and percent of residual errors after excluding sequencing errors
#########################################################################################

score_model<-function(kmer_hist_orig, nls, round){
  x = kmer_hist_orig[[1]]
  y = kmer_hist_orig[[2]]

  pred=predict(nls, newdata=data.frame(x))
  model_sum=summary(nls)
  kcovfloor = floor(min_max(model_sum$coefficients['kmercov',])[[1]])

  ## Compute error rate, by counting kmers unexplained by model through first peak
  ## truncate errors as soon as it goes to zero, dont allow it to go back up
  error_xcutoff = kcovfloor
  error_xcutoff_ind = which(x==error_xcutoff)

  error_kmers = y[1:error_xcutoff_ind] - pred[1:error_xcutoff_ind]

  first_zero = -1

  for (i in 1:error_xcutoff_ind)
  {
    if (first_zero == -1)
    {
      if (error_kmers[i] < 1.0)
      {
        first_zero = i
        if (VERBOSE) { cat(paste("Truncating errors at", i, "\n")) }
      }
    }
    else
    {
      error_kmers[i] = 0
    }
  }

  if (first_zero == -1)
  {
    first_zero = error_xcutoff_ind
  }

  ## The fit is residual sum of square error, excluding sequencing errors
  model_fit_all    = c(sum(as.numeric(y[first_zero:length(y)]     - pred[first_zero:length(y)])     ** 2), first_zero, x[length(y)])
  model_fit_full   = c(sum(as.numeric(y[first_zero:(5*kcovfloor)] - pred[first_zero:(5*kcovfloor)]) ** 2), first_zero, (5*kcovfloor))
  model_fit_unique = c(sum(as.numeric(y[first_zero:(3*kcovfloor)] - pred[first_zero:(3*kcovfloor)]) ** 2), first_zero, (3*kcovfloor))

  ## The score is the percentage of unexplained kmers, excluding sequencing errors
  model_fit_allscore    = c(1-sum(abs(as.numeric(y[first_zero:length(y)]     - pred[first_zero:length(y)])))     / sum(as.numeric(y[first_zero:length(y)])),     first_zero, x[length(y)])
  model_fit_fullscore   = c(1-sum(abs(as.numeric(y[first_zero:(5*kcovfloor)] - pred[first_zero:(5*kcovfloor)]))) / sum(as.numeric(y[first_zero:(5*kcovfloor)])), first_zero, (5*kcovfloor))
  model_fit_uniquescore = c(1-sum(abs(as.numeric(y[first_zero:(3*kcovfloor)] - pred[first_zero:(3*kcovfloor)]))) / sum(as.numeric(y[first_zero:(3*kcovfloor)])), first_zero, (3*kcovfloor))

  fit = data.frame(all  = model_fit_all,      allscore  = model_fit_allscore,
                   full = model_fit_full,     fullscore = model_fit_fullscore,
                   unique = model_fit_unique, uniquescore = model_fit_uniquescore)

  return (fit)
}


## Pick between the two model forms, resolves ambiguity between which is the homozygous and which is the heterozygous peak
###############################################################################

eval_model<-function(kmer_hist_orig, nls1, nls2, round, score_close, score_het_fold_diff){
    nls1score = -1
    nls2score = -1

    ## Evaluate the score the nls1
    if (!is.null(nls1))
    {
      nls1score = score_model(kmer_hist_orig, nls1, round+0.1)

      if(VERBOSE){ cat(paste("nls1score$all:\t", nls1score$all[[1]], "\n"))}
    }
    else
    {
      if (VERBOSE) { cat("nls1score failed to converge\n") }
    }


    ## Evaluate the score of nls2
    if (!is.null(nls2))
    {
      nls2score = score_model(kmer_hist_orig, nls2, round+0.2)

      if(VERBOSE){ cat(paste("nls2score$all:\t", nls2score$all[[1]], "\n"))}
    }
    else
    {
      if (VERBOSE) { cat("nls2score failed to converge\n") }
    }

    ## Return the better of the scores
    if (!is.null(nls1))
    {
      if (!is.null(nls2))
      {
        pdiff = abs(nls1score$all[[1]] - nls2score$all[[1]]) / max(nls1score$all[[1]], nls2score$all[[1]])

        if (pdiff < score_close)
        {
          het1 = summary(nls1)$coefficients['r',][[1]]
          het2 = summary(nls2)$coefficients['r',][[1]]

          if (het2 * score_het_fold_diff < het1)
          {
            if (VERBOSE) { cat(paste("returning nls1, similar score, higher het\n")) }
            return (list(nls1, nls1score))
          }
          else if (het1 * score_het_fold_diff < het2)
          {
            if (VERBOSE) { cat(paste("returning nls2, similar score, higher het\n")) }
            return (list(nls2, nls2score))
          }
        }

        if (nls1score$all[[1]] < nls2score$all[[1]])
        {
          if (VERBOSE) { cat(paste("returning nls1, better score\n")) }
          return (list(nls1, nls1score))
        }
        else
        {
          if (VERBOSE) { cat(paste("returning nls2, better score\n")) }
          return (list(nls2, nls2score))
        }
      }
      else
      {
        if (VERBOSE) { cat(paste("returning nls1, nls2 fail\n")) }
        return (list(nls1, nls1score))
      }
    }

    if (VERBOSE) { cat(paste("returning nls2 by default\n")) }
    return (list(nls2, nls2score))
}


## Wrapper function to try fitting 4 peak model with 2 forms
###############################################################################

estimate_Genome_4peak2<-function(kmer_hist_orig, x, y, k, readlength, round, max_iterations, score_close, score_het_fold_diff){
	## First we see what happens when the max peak is the kmercoverage (typically the homozygous peak) for the plot
	numofReads   = sum(as.numeric(x*y))/(readlength-k+1)
	estKmercov1  = x[which(y==max(y))][1]
	estCoverage1 = estKmercov1*readlength/(readlength-k)
	estLength1   = numofReads*readlength/estCoverage1

    if (VERBOSE) { cat(paste("trying with kmercov: ", estKmercov1, "\n")) }
	nls1    = nls_4peak(x, y, k, estKmercov1, estLength1, max_iterations)
    if (VERBOSE) { print(summary(nls1)) }

	## Second we half the max kmercoverage (typically the heterozygous peak)
	estKmercov2  = estKmercov1 / 2 ##2.5
	estCoverage2 = estKmercov2*readlength/(readlength-k)
	estLength2   = numofReads*readlength/estCoverage2

    if (VERBOSE) { cat(paste("trying with kmercov: ", estKmercov2, "\n")) }
	nls2 = nls_4peak(x, y, k, estKmercov2, estLength2, max_iterations)
    if (VERBOSE) { print(summary(nls2)) }

	return(eval_model(kmer_hist_orig, nls1, nls2, round, score_close, score_het_fold_diff))
}


## Format numbers
###############################################################################
bp_format<-function(num) {
  paste(formatC(round(num),format="f",big.mark=",", digits=0), "bp",sep=" ")
}

percentage_format<-function(num) {
  paste(signif(num,6)*100,"%",sep="")
}
X_format<-function(num) {
  paste(signif(num,4),"X",sep="")
}


## Report results and make plots
###############################################################################

report_results<-function(kmer_hist,kmer_hist_orig, k, container, typical_error) {
    x=kmer_hist_orig[[1]]
    y=kmer_hist_orig[[2]]

	#automatically zoom into the relevant regions of the plot, ignore first 15 positions
    xmax=length(x)
	start=which(y == min(y[1:typical_error]))
	zoomx=x[start:(xmax-1)]
	zoomy=y[start:(xmax-1)]

    ## allow for a little space above max value past the noise
	y_limit = max(zoomy[start:length(zoomy)])*1.1

	x_limit = which(y == max(y[start:length(zoomx)])) * 3

	if (min(zoomy) > zoomy[1]){
		x_limit=max(which(zoomy<zoomy[1])[2],600)
	}

    if (!is.null(container[[1]]))
    {
       model_sum=summary(container[[1]])
       kcov = min_max(model_sum$coefficients['kmercov',])[1]
       x_limit = max(kcov*5.1, x_limit)
    }

    ## Uncomment this to enforce a specific number
    # x_limit=150

    ## Features to report
    het=c(-1,-1)
    total_len=c(-1,-1)
    repeat_len=c(-1,-1)
    unique_len=c(-1,-1)
    dups=c(-1,-1)
    error_rate=c(-1,-1)
    model_status="fail"

    model_fit_unique      = c(0,0,0)
    model_fit_full        = c(0,0,0)
    model_fit_all         = c(0,0,0)
    model_fit_allscore    = c(0,0,0)
    model_fit_fullscore   = c(0,0,0)
    model_fit_uniquescore = c(0,0,0)

    plot_size=2000
    font_size=1.2
    resolution=300

    ## Plot the distribution, and hopefully with the model fit
    linear_plot = plot_ly(kmer_hist_orig) %>% 
        add_segments(x = ~Frequency, xend = ~Frequency, y = 0, yend = ~Count, line = list(color = COLOR_HIST), name = "Observed") %>%
        layout(title = "Count VS Frequency", xaxis = list(range = c(0, x_limit)), yaxis = list(range = c(0, y_limit)))
    # p = plot(kmer_hist_orig, type="n", main="GenomeScope Profile\n", xlab="Coverage", ylab="Frequency", ylim=c(0,
    #     y_limit), xlim=c(0,x_limit),cex.lab=font_size, cex.axis=font_size, cex.main=font_size, cex.sub=font_size)
    # rect(0, 0, max(kmer_hist_orig[[1]])*1.1 , max(kmer_hist_orig[[2]])*1.1, col=COLOR_BGCOLOR)
    # points(kmer_hist_orig, type="h", col=COLOR_HIST, lwd=2)
    ## if(length(kmer_hist[,1])!=length(kmer_hist_orig[,1])){
    ##    abline(v=length(kmer_hist[,1]),col=COLOR_COVTHRES,lty="dashed", lwd=3)
    ##}
    # box(col="black")

    ## Make a second plot in log space over entire range
    log_plot = plot_ly(kmer_hist_orig) %>% 
        add_segments(x = ~Frequency, xend = ~Frequency, y = 1, yend = ~Count, line = list(color = COLOR_HIST), name = "Observed") %>%
        layout(title = "Count VS Frequency", xaxis = list(range = c(0, log10(x_limit)), type = "log"), 
               yaxis = list(range = c(0, log10(y_limit)), type = "log"))
    # plot(kmer_hist_orig, type="n", main="GenomeScope Profile\n", xlab="Coverage", ylab="Frequency", log="xy",cex.lab=font_size, cex.axis=font_size, cex.main=font_size, cex.sub=font_size)
    # rect(1e-10, 1e-10, max(kmer_hist_orig[[1]])*10 , max(kmer_hist_orig[[2]])*10, col=COLOR_BGCOLOR)
    # points(kmer_hist_orig, type="h", col=COLOR_HIST, lwd=2)
    # if(length(kmer_hist[,1])!=length(kmer_hist_orig[,1])){
    #     abline(v=length(kmer_hist[,1]),col=COLOR_COVTHRES,lty="dashed", lwd=3)
    # }
    # box(col="black")

	if (!is.null(container[[1]])) {
        x=kmer_hist[[1]]
        y=kmer_hist[[2]]

        ## The model converged!
        pred=predict(container[[1]], newdata=data.frame(x))

        ## Compute the genome characteristics
        # TODO Might want to include this
        model_sum=summary(container[[1]])

        ## Identify key values
        het  = min_max(model_sum$coefficients['r',])
        dups = min_max(model_sum$coefficients['bias',])
        kcov = min_max(model_sum$coefficients['kmercov',])
        mlen = min_max(model_sum$coefficients['length',])
        md   = min_max(model_sum$coefficients['d',])

        amlen = (mlen[1] + mlen[2]) / 2
        ahet  = (het[1]  + het[2])  / 2
        amd   = (md[1]   + md[2])   / 2
        akcov = (kcov[1] + kcov[2]) / 2
        adups = (dups[1] + dups[2]) / 2

        ## Compute error rate, by counting kmers unexplained by model through first peak
        ## truncate errors as soon as it goes to zero, dont allow it to go back up
        error_xcutoff = floor(kcov[1])
        error_xcutoff_ind = which(x==error_xcutoff)

        error_kmers = y[1:error_xcutoff_ind] - pred[1:error_xcutoff_ind]

        first_zero = -1

        for (i in 1:error_xcutoff_ind) {
         if (first_zero == -1) {
           if (error_kmers[i] < 1.0) {
             first_zero = i
             if (VERBOSE) {
                 cat(paste("Truncating errors at", i, "\n"))
             }
           }
         } else {
           error_kmers[i] = 0
         }
        }

        if (first_zero == -1) {
         first_zero = error_xcutoff_ind
        }

        ## Rather than "0", set to be some very small number so log-log plot looks okay
        error_kmers = pmax(error_kmers, 1e-10)

        total_error_kmers = sum(as.numeric(error_kmers * x[1:error_xcutoff_ind]))
        total_kmers = sum(as.numeric(x*y))

        f1 <- function(x){
             i=seq(1,k)
             h=(1-x)^(k-i)*x^i*choose(k,i)
             sum(h)*total_kmers-total_error_kmers
        }

        error_rate_root = try(uniroot(f1, c(0,1))$root)

        if (class(error_rate_root) == "try-error") {
         error_rate  = c(total_error_kmers/total_kmers/k, total_error_kmers/total_kmers/k)
        } else {
          error_rate  = c(error_rate_root, error_rate_root)
        }

        total_len = (total_kmers-total_error_kmers)/(2*kcov)

        ## find kmers that fit the 2 peak model (no repeats)
        unique_hist <- (2 * (1 - amd) * (1 - (1 - ahet)^k))                         * dnbinom(x, size = akcov     / adups, mu = akcov)     * amlen +
                      ((amd * (1 - (1 - ahet)^k)^2) + (1 - amd) * ((1 - ahet)^k))  * dnbinom(x, size = akcov * 2 / adups, mu = akcov * 2) * amlen

        unique_kmers = sum(as.numeric(x*unique_hist))
        repeat_kmers = total_kmers - unique_kmers - total_error_kmers

        repeat_len=repeat_kmers/(2*kcov)
        unique_len=unique_kmers/(2*kcov)

        score = container[[2]]

        model_fit_allscore    = score$allscore
        model_fit_fullscore   = score$fullscore
        model_fit_uniquescore = score$uniquescore

        model_fit_all    = score$all
        model_fit_full   = score$full
        model_fit_unique = score$unique

        residual = y - pred

        ## Finish Log plot
        #TODO
        # title(paste("\nlen:",  prettyNum(total_len[1], big.mark=","),
        #            "bp",
        #            " uniq:", format(100*(unique_len[1]/total_len[1]), digits=3),
        #            "% ",
        #            " het:",  format(100*ahet, digits=3),
        #            "%",
        #            " kcov:", format(akcov, digits=3),
        #            " err:",   format(100*error_rate[1], digits=3),
        #            "% ",
        #            " dup:",  format(adups, digits=3),
        #            "% ",
        #            " k:",   format(k, digits=3),
        #            sep=""),
        #            cex.main=.85)

        # Sets up data frames
        peak_df = as.data.frame(akcov * c(1, 2, 3, 4))
        x_df = as.data.frame(x)
        x_error_df = as.data.frame(x[1:error_xcutoff_ind])
        names(peak_df) = names(x_df) = names(x_error_df) = "Frequency"
        unique_df = add_column(x_df, Count = unique_hist, type = "Unique Sequence")
        pred_df = add_column(x_df, Count = pred, type = "Full model")
        error_df = add_column(x_error_df, Count = error_kmers, type = "Errors")

        log_plot = add_lines(log_plot, x = ~Frequency, y = ~Count, data = unique_df, line = list(color = COLOR_2PEAK), 
                             name = "Unique sequence")
        log_plot = add_lines(log_plot, x = ~Frequency, y = ~Count, data = pred_df, line = list(color = COLOR_4PEAK), name = "Full model")
        log_plot = add_lines(log_plot, x = ~Frequency, y = ~Count, data = error_df, line = list(color = COLOR_ERRORS), name = "Errors")
        log_plot = add_segments(log_plot, x = ~Frequency, xend = ~Frequency, y = 1, yend = y_limit, data = peak_df,
                                line = list(color = COLOR_KMERPEAK, dash = "dash"), name = "K-mer Peaks")

        if (VERBOSE) { lines(x, residual, col=COLOR_RESIDUAL, lwd=3) }

        ## Add legend
        #TODO
        if(length(kmer_hist[,1])==length(kmer_hist_orig[,1])) {
            # legend(exp(.65 * log(max(x))), 1.0 * max(y),
            #     legend=c("observed", "full model", "unique sequence", "errors", "kmer-peaks"),
            #     lty=c("solid", "solid", "solid", "solid", "dashed"),
            #     lwd=c(3,3,3,3,3),
            #     col=c(COLOR_HIST, COLOR_4PEAK, COLOR_2PEAK, COLOR_ERRORS, COLOR_KMERPEAK),
            #     bg="white")
        } else {
            # legend("topright",
            #     ##legend(exp(.65 * log(max(x))), 1.0 * max(y),
            #     legend=c("observed", "full model", "unique sequence", "errors", "kmer-peaks","cov-threshold"),
            #     lty=c("solid", "solid", "solid", "solid", "dashed", "dashed"),
            #     lwd=c(3,3,3,3,2,3),
            #     col=c(COLOR_HIST, COLOR_4PEAK, COLOR_2PEAK, COLOR_ERRORS, COLOR_KMERPEAK, COLOR_COVTHRES),
            #     bg="white")
        }

        # dev.set(dev.next())

        ## Finish Linear Plot
        #TODO
        # title(paste("\nlen:",  prettyNum(total_len[1], big.mark=","),
        #            "bp",
        #            " uniq:", format(100*(unique_len[1]/total_len[1]), digits=3),
        #            "% ",
        #            " het:",  format(100*ahet, digits=3),
        #            "%",
        #            " kcov:", format(akcov, digits=3),
        #            " err:",   format(100*error_rate[1], digits=3),
        #            "% ",
        #            " dup:",  format(adups, digits=3),
        #            "% ",
        #            " k:",   format(k, digits=3),
        #            sep=""),
        #            cex.main=.85)

        # Sets up data frames
        peak_df = as.data.frame(akcov * c(1, 2, 3, 4))
        x_df = as.data.frame(x)
        x_error_df = as.data.frame(x[1:error_xcutoff_ind])
        names(peak_df) = names(x_df) = names(x_error_df) = "Frequency"
        unique_df = add_column(x_df, Count = unique_hist, type = "Unique Sequence")
        pred_df = add_column(x_df, Count = pred, type = "Full model")
        error_df = add_column(x_error_df, Count = error_kmers, type = "Errors")
        
        linear_plot = add_lines(linear_plot, x = ~Frequency, y = ~Count, data = unique_df, line = list(color = COLOR_2PEAK), 
                                name = "Unique sequence")
        linear_plot = add_lines(linear_plot, x = ~Frequency, y = ~Count, data = pred_df, line = list(color = COLOR_4PEAK), 
                                name = "Full model")
        linear_plot = add_lines(linear_plot, x = ~Frequency, y = ~Count, data = error_df, line = list(color = COLOR_ERRORS), 
                                name = "Errors")
        linear_plot = add_segments(linear_plot, x = ~Frequency, xend = ~Frequency, y = 0, yend = y_limit, data = peak_df, 
                                   line = list(color = COLOR_KMERPEAK, dash = "dash"), name = "K-mer Peaks")

        if (VERBOSE) { lines(x, residual, col=COLOR_RESIDUAL, lwd=3) }

        ## Add legend
        #TODO
        # legend(.65 * x_limit, 1.0 * y_limit,
        #       legend=c("observed", "full model", "unique sequence", "errors", "kmer-peaks"),
        #       lty=c("solid", "solid", "solid", "solid", "dashed"),
        #       lwd=c(3,3,3,3,2),
        #       col=c(COLOR_HIST, COLOR_4PEAK, COLOR_2PEAK, COLOR_ERRORS, COLOR_KMERPEAK),
        #       bg="white")

        model_status="done"

        #TODO
        # cat(paste("Model converged het:", format(ahet, digits=3),
        #          " kcov:", format(akcov, digits=3),
        #          " err:", format(error_rate[1], digits=3),
        #          " model fit:", format(adups, digits=3),
        #          " len:", round(total_len[1]), "\n", sep=""))
	} else {
        # linear_plot = linear_plot + scale_colour_manual(values = c("Observed" = COLOR_HIST))
        # log_plot = log_plot + scale_colour_manual(values = c("Observed" = COLOR_HIST))

        # title("\nFailed to converge")
        # dev.set(dev.next())
        # title("\nFailed to converge")
        # cat("Failed to converge\n")
    }

    size = mean(c(total_len[2], total_len[1]))
    summaryTable = matrix(c(percentage_format(het[1]), percentage_format(het[2]),
        bp_format(total_len[2]), bp_format(total_len[1]),
        bp_format(repeat_len[2]), bp_format(repeat_len[1]),
        bp_format(unique_len[2]), bp_format(unique_len[1]),
        percentage_format(model_fit_allscore[1]), percentage_format(model_fit_allscore[1]),
        percentage_format(error_rate[1]), percentage_format(error_rate[2])), ncol = 2, byrow = TRUE)
    colnames(summaryTable) = c("Minimum", "Maximum")
    rownames(summaryTable) = c("Heterozygosity", "Genome Haploid Length", "Genome Repeat Length",
        "Genome Unique Length", "Model Fit", "Read Error Rate")
    summaryTable = as.data.frame(summaryTable)

    return (list("linear_plot" = linear_plot, "log_plot" = log_plot, "size" = size, "summary" = summaryTable))
}

runGenomeScope <- function(kmer_prof, k, readlength, maxCovGenomeLen, num_rounds, start_shift, typical_error, max_iterations, 
                           score_close, score_het_fold_diff) {
    # print("======RUNNING GENOMESCOPE======")
    
    minkmerx = 1;
    if (kmer_prof[1,1] == 0) {
        if (VERBOSE) { cat("Histogram starts with zero, reseting minkmerx\n");  }
        minkmerx = 2;
    }

	kmer_prof <- kmer_prof[c(minkmerx:(length(kmer_prof[,2])-1)),] #get rid of the last position
    kmer_prof_orig <- kmer_prof

    ## try to find the local minimum between errors and the first (heterozygous) peak
    start <- which(kmer_prof[,2]==min(kmer_prof[1:typical_error,2]))

    maxCovIndex = -1

    ## Figure out which kmers to exclude, if any
    if(maxCovGenomeLen == -1){
        maxCovIndex <- length(kmer_prof[,1])
    }
    else
    {
        ## Figure out the index we should use for this coverage length
        x <- kmer_prof[,1]
        maxCovIndex <- length(x[x<=maxCovGenomeLen])
    }

    if (VERBOSE) { cat(paste("using maxCovGenomeLen:", maxCovGenomeLen, " with index:", maxCovIndex, "trying 4peak model... \n")) }

    ## terminate after NUM_ROUND iterations, store best result so far in container
	round <- 0
	best_container <- list(NULL,0)

	while(round < num_rounds) {
        if (VERBOSE) { cat(paste("round", round, "trimming to", start, "trying 4peak model... \n")) }

        ## Reset the input trimming off low frequency error kmers
        kmer_prof=kmer_prof_orig[1:maxCovIndex,]
        x <- kmer_prof[start:maxCovIndex,1]
        y <- kmer_prof[start:maxCovIndex,2]

        model_4peaks <- estimate_Genome_4peak2(kmer_prof, x, y, k, readlength, round, max_iterations, score_close, score_het_fold_diff)

		#check if this result is better than previous
        if (!is.null(model_4peaks[[1]])) {
          if (is.null(best_container[[1]])) {
            if (VERBOSE) { cat(paste("no previous best, updating best")) }
            best_container = model_4peaks
          } else {
            pdiff = abs(model_4peaks[[2]]$all[[1]] - best_container[[2]]$all[[1]]) / max(model_4peaks[[2]]$all[[1]], best_container[[2]]$all[[1]])

            if (pdiff < score_close) {
              hetm = summary(model_4peaks[[1]])$coefficients['r',][[1]]
              hetb = summary(best_container[[1]])$coefficients['r',][[1]]

              if (hetb * score_het_fold_diff < hetm) {
                if (VERBOSE) { cat(paste("model has significantly higher heterozygosity but similar score, overruling")) }
                best_container = model_4peaks
              } else if (hetm * score_het_fold_diff < hetb) {
                if (VERBOSE) { cat(paste("previous best has significantly higher heterozygosity and similar score, keeping")) }
              } else if (model_4peaks[[2]]$all[[1]] < best_container[[2]]$all[[1]]) {
                if (VERBOSE) { cat(paste("score is marginally better but het rate is not extremely different, upating")) }
                best_container = model_4peaks
              }
            } else if (model_4peaks[[2]]$all[[1]] < best_container[[2]]$all[[1]]) {
              if (VERBOSE) { cat(paste("score is significantly better, upating")) }
              best_container = model_4peaks
            }
          }
        }

        ## Ignore a larger number of kmers as errors
        start <- start + start_shift
		round <- round + 1
	}
    ## Report the results, note using the original full profile
	r = report_results(kmer_prof,kmer_prof_orig, k, best_container, typical_error)
    return (r)
}



## Main program starts here
###############################################################################

# args<-commandArgs(TRUE)
#
# if(length(args) < 4) {
# 	cat("USAGE: genomescope.R histogram_file k-mer_length read_length output_dir [kmer_max] [verbose]\n")
# } else{
#
#     ## Load the arguments from the user
# 	histfile   <- args[[1]]
# 	k          <- as.numeric(args[[2]])
# 	readlength <- as.numeric(args[[3]])
# 	foldername <- args[[4]]
#
#     maxCovGenomeLen = -1
#
#     if ((length(args) >= 5)) {
#         maxCovGenomeLen = as.numeric(args[[5]])
#     }
#
#     if ((length(args) == 6) && (as.numeric(args[[6]] == 1))) { VERBOSE = 1 }
#
#     ## values for testing
#     #histfile <- "~/build/genomescope/simulation/simulation_results/Arabidopsis_thaliana.TAIR10.26.dna_sm.toplevel.fa_het0.01_br1_rl100_cov100_err0.01_reads.fa21.hist"
#     #k <- 21
#     #readlength <- 100
#     #foldername <- "~/build/genomescope/simulation/simulation_analysis/Arabidopsis_thaliana.TAIR10.26.dna_sm.toplevel.fa_het0.01_br1_rl100_cov100_err0.01_reads.fa21.hist"
#
#     if (k > readlength) { stop("K cannot be greater than readlength") }
#
#     cat(paste("GenomeScope analyzing ", histfile, " k=", k, " readlen=", readlength, " outdir=", foldername, "\n", sep=""))
#
# 	dir.create(foldername, showWarnings=FALSE)
#
# 	runGenomeScope(histfile, k, readlength, foldername, maxCovGenomeLen)
# }
