base_dir = "~/repos/bm-CompAspCboost"

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

## Data configurations:
## ---------------------------------------------------

for (i in seq_len(50L)) {
  runs = 50L

  ns = c(5000L, 10000L, 20000L, 50000L, 100000L)
  ps = c(5L, 10L, 20L, 50L)
  pnoise_rel = c(0.5, 1, 2, 5)
  sn_ratio = c(0.1, 1, 10)
  reps = seq_len(20L)
  mom = runif(n = 1L, min = 0.0000001, max = 0.1)

  df_configs = expand.grid(n = ns, p = ps, pnoise_rel = pnoise_rel,
    sn_ratio = sn_ratio, rep = reps, mom = mom)

  # Add number of noise features as absolute number:
  df_configs$pnoise = trunc(df_configs$p * df_configs$pnoise_rel)
  df_configs$pnoise_rel = NULL

  df_configs = df_configs[sample(nrow(df_configs), runs),]

  ## Files of the benchmark scripts:
  ## ---------------------------------------------------

  bm_dirs = paste0(base_dir, "/bm-scripts/optimizer/agbm-mom")

  runBM(df_configs, bm_dirs, cores = 2L)
}
