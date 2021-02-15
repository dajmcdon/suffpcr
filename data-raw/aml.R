## code to prepare `aml` dataset goes here
library(tidyverse)
a1 <- read_csv("~/Documents/Work/Manuscripts/superCSPCA/ISMB-2021/code-LeiD/real/AML/rawdata/AML_with_gene_names.csv")
aw <- read.table("~/Documents/Work/Manuscripts/superCSPCA/ISMB-2021/code-LeiD/real/AML/rawdata/Bullinger2004_expression_data.txt", sep = ",")
a1y <- read.table("~/Documents/Work/Manuscripts/superCSPCA/ISMB-2021/code-LeiD/real/AML/rawdata/Bullinger2002_survival_data.txt", sep = ",")
aml <- list()
aml$gene_info <- a1$NAME
aml$x <- t(as.matrix(aw))
aml$survival_time <- a1y$V1
aml$survival_status <- a1y$V2
usethis::use_data(aml, overwrite = TRUE)
