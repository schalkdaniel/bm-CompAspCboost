base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/binning/runtime")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

config = loadConfig(base_sub_dir)

nm_save = paste0("xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, ".Rda")


## Write compboost code here:
## ------------------------------------







## ------------------------------------



## Save results:
save(nm_save, file = paste0(base_sub_dir, "/", nm_save))

