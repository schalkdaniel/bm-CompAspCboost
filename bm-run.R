source("R/bm-sim-data.R")
source("R/bm-run.R")

## Data configurations:
## ---------------------------------------------------

#ns = c(5000L, 10000L, 20000L, 50000L, 100000L)
ns = c(5000L)
#ps = c(5L, 10L, 20L, 50L)
ps = c(5L, 10L)
#pnoise_rel = c(0.5, 1, 2, 5)
pnoise_rel = c(0.5, 1)
#sn_ratio = c(0.1, 1, 10)
sn_ratio = c(0.1, 1)
#reps = seq_len(20L)
reps = seq_len(2L)

df_configs = expand.grid(n = ns, p = ps, pnoise_rel = pnoise_rel,
  sn_ratio = sn_ratio, rep = reps)

# Add number of noise features as absolute number:
df_configs$pnoise = trunc(df_configs$p * df_configs$pnoise_rel)
df_configs$pnoise_rel = NULL

## Files of the benchmark scripts:
## ---------------------------------------------------

#bm_dirs = paste0("bm-scripts/binning/", c("memory", "runtime", "performance"))
bm_dirs = paste0("bm-scripts/binning/", c("runtime", "performance"))

runBM(df_configs, bm_dirs)
