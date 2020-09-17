source("R/bm-sim-data.R")
source("R/bm-run.R")

## Data configurations:
## ---------------------------------------------------

#ns = c(5000L, 10000L, 20000L, 50000L, 100000L)
#ps = c(5L, 10L, 20L, 50L)
#pnoise_rel = c(0.5, 1, 2, 5)
#sn_ratio = c(0.1, 1, 10)
#reps = seq_len(20L)

ns = 5000L
ps = 5L
pnoise_rel = c(1, 2)
sn_ratio = 1
reps = seq_len(20L)

df_configs = expand.grid(n = ns, p = ps, pnoise_rel = pnoise_rel,
  sn_ratio = sn_ratio, rep = reps)

# Add number of noise features as absolute number:
df_configs$pnoise = trunc(df_configs$p * df_configs$pnoise_rel)
df_configs$pnoise_rel = NULL

## Files of the benchmark scripts:
## ---------------------------------------------------

#bm_dirs = paste0("bm-scripts/binning/", c("memory", "runtime",q"performance"))
#bm_dirs = paste0("bm-scripts/binning/", c("runtime", "performance"))
#bm_dirs = paste0("bm-scripts/categorical/", c("performance"))
#bm_dirs = paste0("bm-scripts/categorical/", c("memory"))
#bm_dirs = "bm-scripts/optimizer/performance"
bm_dirs = "bm-scripts/optimizer/memory"

runBM(df_configs, bm_dirs, cores = 2L)
