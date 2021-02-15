## code to prepare `vantveer` dataset goes here
x <- read_csv("~/Documents/Work/Manuscripts/superCSPCA/ISMB-2021/code-LeiD/real/BreastCancer1/rawdata/vantVeer2002_expression_data.CSV", col_names = FALSE)
y <- read_csv("~/Documents/Work/Manuscripts/superCSPCA/ISMB-2021/code-LeiD/real/BreastCancer1/rawdata/vantVeer2002_survival_data.CSV", col_names = FALSE)
info <- read_csv("~/Documents/Work/Manuscripts/superCSPCA/ISMB-2021/code-LeiD/real/BreastCancer1/rawdata/vantVeer2002_gene_names.CSV", col_names = FALSE)
vantveer <- list()
vantveer$gene_info <- info$X1
vantveer$x <- t(as.matrix(x))
vantveer$survival_time <- y$X1
vantveer$survival_status <- y$X2



usethis::use_data(vantveer, overwrite = TRUE)
