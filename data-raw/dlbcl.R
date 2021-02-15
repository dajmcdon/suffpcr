## code to prepare `dlbcl` dataset goes here
staudt.x = matrix(scan("../ISMB-2021/code-LeiD/real/DLBCL/rawData/staudt.x"), ncol=240, byrow=TRUE)
staudt.tim = scan("../ISMB-2021/code-LeiD/real/DLBCL/rawData/staudt.tim")
staudt.stat = scan("../ISMB-2021/code-LeiD/real/DLBCL/rawData/staudt.status")
dlbcl = list()
dlbcl$x = t(staudt.x)
dlbcl$survival_time = staudt.tim
dlbcl$survival_status = staudt.stat
load("~/Desktop/dlbcl-groups-raw.Rdata")
dlbcl$gene_info = as_tibble(dlbcl_groups)

usethis::use_data(dlbcl, overwrite = TRUE)
