## code to prepare `miller` dataset goes here
load("../ISMB-2021/code-LeiD/real/BreastCancer2/rawdata/uppsala-U133A-50_.RData")
miller = list()
miller$x = t(upp.x)
miller$clinical = upp.clinical


usethis::use_data(miller, overwrite = TRUE)
