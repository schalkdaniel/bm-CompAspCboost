source("bm-functions.R")

## Data configurations:
ns = c(5000L, 10000L, 20000L, 50000L, 100000L)
ps = c(5L, 10L, 20L, 50L)
pnoise_rel = c(0.5, 1, 2, 5)
sn_ratio = c(0.1, 1, 10)
reps = seq_len(20L)

df_configs = expand.grid(ns, ps, pnoise_rel, sn_ratio, reps)




