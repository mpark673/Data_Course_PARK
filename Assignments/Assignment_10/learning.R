library(tidyverse)
library(easystats)
library(plotly)
library(kableExtra)
library(ShortRead)
library(Biostrings)

#pharokka - command line tool annotation for genes

# for sequence alignment DECIPHER package not on CRAN, look for it on google
# phangorn package has tutorials to build phylogenetic trees, ggtree, ape packages for visualizing trees
# ShortRead::readFasta will allow me to read in fasta files
# may look like a character class, but it is actually a DNAStringSet

BiocManager::install("DECIPHER")

untar("../../Data/Phagefasta/genome_assemblies_genome_fasta.tar",exdir = "../../Data/Phagefasta")

path <- list.files(path = "../../Data/Phagefasta/sphagefastas",
                   recursive = TRUE,
                   pattern = ".gz",
                     full.names = TRUE,
                   ignore.case = TRUE)

rfa <- readFasta(path)

dss <- readDNAStringSet(path)
dss
reverseComplement(dss)

gccontent <- letterFrequency(dss, letters = "GC", as.prob = TRUE)
mean(gccontent)
range(gccontent)
sd(gccontent)

dss
dss[which(gccontent == max(gccontent))]


# ideas: aligning phages to hosts bases on GC content as phage integration requires high GC content alignment
# Aligning all phages against known antibiotic synergistic phages? Specific Gene? I have been looking for phage genes
# specifically aligned with antibiotic synergy but I haven't found any yet. Another option is aligning sequences with
# my target host in lab as a "precursor" final project.

pss <- translate(dss)


pss2 <- pss[1:3]

decipher_alignment <- DECIPHER::AlignSeqs(pss2, processors = parallel::detectCores() - 2,verbose = TRUE)

adj_decipher_alignment <- DECIPHER::AdjustAlignment(decipher_alignment,processors = parallel::detectCores() - 1)