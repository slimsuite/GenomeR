# BINF3111 17s2 - Genomer

This repository contains the R scripts necessary to run the R Shiny app Genomer.
In order to run this app please install the dependencies following these instructions

## What is this repository for?

* Quick summary
* Version
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

## Getting Started

Genomer takes in a histogram of k-mer frequencies as its input. You can use tools such as [Jellyfish](http://www.genome.umd.edu/jellyfish.html) to compute the histogram. Below is an example command of running Jellyfish:

```
jellyfish count -m 21 -s 100M -t 10 -C reads.fasta && jellyfish histo -t 10 reads.jf > reads.histo
```

To install the depencies for Genomre, run the following command in an R session:

```
install.packages(c("shiny", "shinyjs"))
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