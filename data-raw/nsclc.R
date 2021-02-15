## code to prepare `nsclc` dataset goes here
load("../ISMB-2021/code-LeiD/real/NSCLC/rawdata/Chemores-clindat-public-Apr2014.RData")
nsclc = list()
nsclc$clinical = chemores.clindat
load("../ISMB-2021/code-LeiD/real/NSCLC/rawdata/Chemores-miRNA-LVS-normalized.RData")
nsclc$x = t(mirdat)


usethis::use_data(nsclc, overwrite = TRUE)
