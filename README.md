# BINF3111 17s2 - Genomer

This repository contains the R scripts necessary to run the R Shiny app Genomer.
In order to run this app please install the dependencies following these instructions

## Introduction to Genomer

Genomer estimates the size of their sequencing data using three different algorithms and allows users to interact 
with the resulted models and compare between the results of the algorithms

Below is a list of algorithms included:

* **Simple Counting Method** - Simply counts the number of k-mers
* **Peak K-mer Identifying Method** - Identifies the peak of the k-mer distribution and estimates the size of the genome
* [**GenomeScope**](https://github.com/schatzlab/genomescope) - Estimates the overall characteristics of a genome

## Getting Started

Genomer takes in a histogram of k-mer frequencies as its input. You can use tools such as [Jellyfish](http://www.genome.umd.edu/jellyfish.html) to compute the histogram. Below is an example command of running Jellyfish:

```
jellyfish count -m 21 -s 100M -t 10 -C reads.fasta && jellyfish histo -t 10 reads.jf > reads.histo
```

To install the depencies for Genomre, run the following command in an R session:

```
install.packages(c("shiny", "shinyjs", "shinyWidgets", "plotly", "ggplot2"))
```

### Running Genomer

To run Genomer, simply run the following colmmand in an R session (replace <GENOMER_DIR> with your own directory of Genomer):

```
shiny::runApp('<GENOMER_DIR>')
```

* Summary of set up
* Configuration
* Dependencies
* Database configuration
* How to run tests
* Deployment instructions

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact