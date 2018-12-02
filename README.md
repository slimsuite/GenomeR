# BINF3111 17s2 - Genomer

## [Wiki Page](https://bitbucket.org/violetbrina/binf3111-genomer/wiki/Home)

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

### Running Genomer

To run Genomer, simply run the following command in an R session (replace <GENOMER_DIR> with your own directory of Genomer):

```
shiny::runApp('<GENOMER_DIR>')
```
