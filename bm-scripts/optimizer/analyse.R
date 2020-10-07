library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(viridis)

source("../../R/bm-sim-data.R")
source("../../R/helper.R")

sysfonts::font_add("Gyre Bonum",
    regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
    bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")
showtext::showtext_auto()

font_scale = 3

## memory
## ----------------------------------------------

# Load local data:
# -------------------

files = c(list.files("memory", full.name = TRUE), list.files("../binning/memory", full.name = TRUE))
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

mem_setup = 50

for (fn in files) {
  #load(paste0("memory/", fn))
  load(fn)
  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)
  dat_noise = dat$data

  set.seed(bm_extract$data_seed * bm_extract$config$rep)
  dat_noise$y = rnorm(n = bm_extract$config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / bm_extract$config$sn_ratio)

  mem_rsim = sum(c(object.size(dat), object.size(dat_noise))) / 1024^2

  if (grepl("binning", fn)) {
    ll_rows[[k]] = data.frame(
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      #rep         = bm_extract$config$rep,    # rep is always 1 for memory
      ncolsnoise  = bm_extract$config$pnoise,
      mem         = last(bm_extract$ms_extract_nobinning$mem_heap_B) - mem_setup - mem_rsim,
      unit        = last(bm_extract$ms_extract_nobinning$unit),
      method      = "COD"
    )
  } else {
    ll_rows[[k]] = data.frame(
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      #rep         = bm_extract$config$rep,    # rep is always 1 for memory
      ncolsnoise  = bm_extract$config$pnoise,
      mem         = last(bm_extract$ms_extract_agbm$mem_heap_B) - mem_setup - mem_rsim,
      unit        = last(bm_extract$ms_extract_agbm$unit),
      method      = "AGBM"
    )
  }
  k = k+1
}
df_optim_memory = do.call("rbind", ll_rows)


# Plot real memory as lines:
# --------------------------

## NOT RELEVANT!!!

#gg = df_binning_memory %>%
#df_binning_memory %>%
  #ggplot(aes(x = nrows, y = mem / 1024, color = method)) +
    #geom_line() +
    #geom_point() +
    #theme_minimal(base_family = "Gyre Bonum") +
    #theme(
      #strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      #strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      #axis.text.x = element_text(angle = 45, vjust = 0.5),
      #axis.text = element_text(size = 8 * font_scale),
      #axis.title = element_text(size = 10 * font_scale)
    #) +
    #scale_x_continuous(breaks = sort(unique(df_binning_memory$nrows))[-c(1,2)]) +
    #scale_color_viridis(discrete = TRUE) +
    #xlab("Number of Rows") +
    #ylab("Allocated Memory in GB") +
    #labs(color = "") +
    #facet_grid(ncolsnoise ~ ncols, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "memory_lines.pdf", width = dinA4width * 2/3 * 0.6, height = dinA4width * 2/3, units = "mm")



# Plot used memory (proportional):
# --------------------------------

df_plt_mem = df_optim_memory %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(mem)) %>%
  group_by(nrows, ncolsnoise, ncols) %>%
  summarize(rel_mem = mean_mem[method == "COD"] / mean_mem[method == "AGBM"], ptotal = ncols[1] + ncolsnoise[1])

df_plt_mem$ptotal = factor(df_plt_mem$ptotal, levels = as.character(sort(unique(df_plt_mem$ptotal))))

gg = ggplot(data = df_plt_mem, aes(x = nrows, y = rel_mem, color = ptotal, group = paste0(ncols, ncolsnoise))) +
  geom_hline(yintercept = 1, col = "dark red", lty = 2) +
  geom_line() +
  geom_point() +
  theme_minimal(base_family = "Gyre Bonum") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale)
  ) +
  scale_x_continuous(breaks = sort(unique(df_optim_memory$nrows))) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of Rows") +
  ylab("Relative Allocated Memory") +
  labs(color = "Number of\nFeatures") #+
  #facet_grid(. ~ ncols, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "optim_memory_rel_lines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")

tmp = df_optim_memory %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(mem)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  group_by(nrows, ptotal) %>%
  select(nrows, ptotal, COD, AGBM) %>%
  filter(COD == max(COD)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal) %>%
  mutate(rel = COD / AGBM)

knitr::kable(round(tmp, 2), format = "latex")



## runtime
## ----------------------------------------------

files = list.files("runtime")
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  load(paste0("runtime/", fn))
  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    time_init   = c(bm_extract$time_cod["init.elapsed"], bm_extract$time_agbm["init.elapsed"]),
    time_fit   = c(bm_extract$time_cod["fit.elapsed"], bm_extract$time_agbm["fit.elapsed"]),
    method      = c("COD", "AGBM")
  )
  k = k+1
}
df_optim_runtime= do.call("rbind", ll_rows)


df_plt_run = df_optim_runtime %>%
  mutate(time = time_init + time_fit) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise) %>%
  summarize(
    rel_time_init = time_init[method == "COD"] / time_init[method == "AGBM"],
    rel_time_fit = time_fit[method == "COD"] / time_fit[method == "AGBM"],
    rel_time = time[method == "COD"] / time[method == "AGBM"],
    ptotal = ncols[1] + ncolsnoise[1]
  ) %>%
  gather(key = "phase", value = "rel_time", starts_with("rel_time"))

df_plt_run$phase[df_plt_run$phase == "rel_time"] = "Initialization + Fitting"
df_plt_run$phase[df_plt_run$phase == "rel_time_init"] = "Initialization"
df_plt_run$phase[df_plt_run$phase == "rel_time_fit"] = "Fitting"
df_plt_run$phase = factor(df_plt_run$phase, levels = c("Initialization + Fitting", "Initialization", "Fitting"))

df_plt_run$ptotal = factor(df_plt_run$ptotal, levels = as.character(sort(unique(df_plt_run$ptotal))))


#gg = ggplot(data = df_plt_run %>% filter(rel_time < 10, rel_time > 1), aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal))) +
gg = ggplot(data = df_plt_run, aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal))) +
  geom_hline(yintercept = 1, lty = 2, col = "dark red") +
  geom_violin(alpha = 0.2) +
  theme_minimal(base_family = "Gyre Bonum") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE) +
  xlab("Number of Rows") +
  ylab("Relative Speedup") +
  labs(color = "Number of\nFeatures", fill = "Number of\nFeatures") +
  facet_grid(. ~ phase, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "optim_runtime_rel_violines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")

tmp = df_optim_runtime %>%
  mutate(time = time_init + time_fit) %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(time)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  group_by(nrows, ptotal) %>%
  select(nrows, ptotal, COD, AGBM) %>%
  filter(COD == max(COD)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal) %>%
  mutate(rel = COD / AGBM)

tmp$COD = tmp$COD / 60
tmp$AGBM = tmp$AGBM / 60

knitr::kable(round(tmp, 2), format = "latex")





## performance
## ----------------------------------------------


files = list.files("agbm-mom", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
ll_rows_paths = list()
k = 1

for (fn in files) {
  cat("Read - ", k, "/", length(files), "\n")

  load(fn)

  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)
  oob_int = mean((mean(dat$data$y) - dat$data$y)^2)

  ll_rows[[k]] = data.frame(
    file        = fn,
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    time_init   = c(bm_extract$time_cod["init.elapsed"], bm_extract$time_agbm["init.elapsed"]),
    time_fit   = c(bm_extract$time_cod["fit.elapsed"], bm_extract$time_agbm["fit.elapsed"]),
    method      = c("COD", "AGBM"),
    #iterations  = c(length(bm_extract$log_cod$oob), length(bm_extract$log_agbm$oob)),
    iterations  = c(which.min(bm_extract$log_cod$oob), which.min(bm_extract$log_agbm$oob)),
    min_oob     = c(min(bm_extract$log_cod$oob), min(bm_extract$log_agbm$oob)),
    mom         = bm_extract$momentum,
    #oob_int_min = c(-diff(c(bm_extract$log_cod$oob[1], min(bm_extract$log_cod$oob))), -diff(c(bm_extract$log_agbm$oob[1], min(bm_extract$log_agbm$oob))))
    oob_int_min = c(-diff(c(oob_int, min(bm_extract$log_cod$oob))), -diff(c(oob_int, min(bm_extract$log_agbm$oob))))
  )
  #oob_cod = bm_extract$log_cod$oob
  #oob_agbm = bm_extract$log_agbm$oob

  #iter_cod = as.integer(seq(1, length(oob_cod), length.out = 100L))
  #iter_agbm = as.integer(seq(1, length(oob_agbm), length.out = 100L))

  #ll_rows_paths[[k]] = data.frame(
    #file        = fn,
    #date        = bm_extract$date,
    #data_seed   = bm_extract$data_seed,
    #nrows       = bm_extract$config$n,
    #ncols       = bm_extract$config$p,
    #sn_ratio    = bm_extract$config$sn_ratio,
    #rep         = bm_extract$config$rep,
    #ncolsnoise  = bm_extract$config$pnoise,
    #method      = c(rep("COD", length(iter_cod)), rep("AGBM", length(iter_agbm))),
    #oob         = c(oob_cod[iter_cod], oob_agbm[iter_agbm]),
    #iter        = c(iter_cod, iter_agbm)
  #)
  k = k+1
}
df_agbm = do.call("rbind", ll_rows)
#df_agbm_paths = do.call("rbind", ll_rows_paths)
#df_agbm$time = df_agbm$time_init + df_agbm$time_fit

df_plt = df_agbm %>%
  mutate(time = time_init + time_fit) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, mom) %>%
  summarize(
    #rel_time_init = time_init[method == "COD"] / time_init[method == "AGBM"],
    #rel_time_fit = time_fit[method == "COD"] / time_fit[method == "AGBM"],
    rel_time = time[method == "COD"] / time[method == "AGBM"],
    ptotal = ncols[1] + ncolsnoise[1],
    diffiter = (iterations[method == "COD"] - iterations[method == "AGBM"]) / iterations[method == "COD"],
    diffoob  = (min_oob[method == "COD"] - min_oob[method == "AGBM"]) / min_oob[method == "COD"],
    diffiter_t = (iterations[method == "COD"] - iterations[method == "AGBM"]),
    diffoob_t  = (min_oob[method == "COD"] - min_oob[method == "AGBM"]),
    diffoob_int = (oob_int_min[method == "COD"] - oob_int_min[method == "AGBM"]) / oob_int_min[method == "COD"],
    diffoob_int_t = oob_int_min[method == "COD"] - oob_int_min[method == "AGBM"],
    range_cod = oob_int_min[method == "COD"],
    iter_cod = iterations[method == "COD"],
    range_agbm = oob_int_min[method == "AGBM"]
  )

#mod_iter = lm(diffiter ~ mom + nrows + ncols + ncolsnoise + as.factor(sn_ratio), data = df_plt)
#summary(mod_iter)
#mod_oob = lm(diffoob ~ mom + nrows + ncols + ncolsnoise  + as.factor(sn_ratio), data = df_plt)
#summary(mod_oob)


#ggplot(df_plt, aes(x = diffiter_t, y = diffoob_int, color = as.factor(ncols))) +
  #geom_point(alpha = 1, size = 2) +
  #facet_grid(nrows ~ sn_ratio, scales = "free_y") +
  #scale_color_viridis(discrete = TRUE)


gg = ggplot() +
  geom_point(data = df_plt %>% filter(diffiter > -1, iter_cod < 20000, !((sn_ratio == 1) & (diffiter < 0.6)), !((sn_ratio == 10) & (diffiter < 0.75))),
    aes(x = diffiter, y = range_cod, color = as.factor(ncols)), alpha = 0.3, stroke = 1) +
  geom_segment(data = df_plt %>% filter(diffiter > -1, iter_cod < 20000, !((sn_ratio == 1) & (diffiter < 0.6)), !((sn_ratio == 10) & (diffiter < 0.75))),
    aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = as.factor(ncols)), alpha = 1) +
  #geom_point(data = df_plt, aes(x = diffiter, y = range_cod, color = as.factor(ncols)), alpha = 0.3, stroke = 0) +
  #geom_segment(data = df_plt, aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = as.factor(ncols)), alpha = 1) +
  geom_point(data = df_plt %>% filter(diffiter > -1, iter_cod == 20000, !((sn_ratio == 1) & (diffiter < 0.6)), !((sn_ratio == 10) & (diffiter < 0.75))), aes(x = diffiter, y = range_agbm, color = as.factor(ncols)), alpha = 0.3, stroke = 1, shape = 4, show.legend = FALSE) +
  facet_grid(nrows ~ sn_ratio, scales = "free") +
  #scale_color_viridis(discrete = TRUE)
  scale_color_brewer(palette = "Set1") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale)
  ) +
  xlab("Relative Improvement of Iterations") +
  ylab("Risk Improvement from\nIntercept Model") +
  labs(color = "Number of informative\nfeatures")


#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "optim_oob.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")



## Try to find a model for momentum:

df_model = df_agbm %>%
  filter(method == "AGBM") %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  select(nrows, ptotal, mom, sn_ratio, ncols, ncolsnoise)

mod_mom = lm(mom ~ nrows + ncols + ncolsnoise + as.factor(sn_ratio), data = df_model)
summary(mod_mom)

ggplot(data = df_model, aes(x = nrows, y = mom, color = as.factor(ncols))) + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ ncolsnoise)


ggplot() +
  geom_point(data = df_plt %>% filter(diffiter > 0, iter_cod < 20000), aes(x = diffiter, y = range_cod, color = mom), alpha = 0.3, stroke = 1) +
  geom_segment(data = df_plt %>% filter(diffiter > 0, iter_cod < 20000), aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = mom), alpha = 1) +
  #geom_point(data = df_plt, aes(x = diffiter, y = range_cod, color = as.factor(ncols)), alpha = 0.3, stroke = 0) +
  #geom_segment(data = df_plt, aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = as.factor(ncols)), alpha = 1) +
  geom_point(data = df_plt %>% filter(diffiter > 0, iter_cod == 20000), aes(x = diffiter, y = range_agbm, color = mom), alpha = 0.3, stroke = 1, shape = 4, show.legend = FALSE) +
  facet_grid(nrows ~ sn_ratio, scales = "free") +
  #scale_color_viridis(discrete = TRUE)
  #scale_color_brewer(palette = "Set1") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale)
  ) +
  xlab("Relative Improvement of Iterations") +
  ylab("Risk Improvement from\nIntercept Model") +
  labs(color = "Number of informative\nfeatures")






# Performance: Visualize one setting:
files = list.files("agbm-mom", full.names = TRUE)
files = files[grep("xxx", files)]

fn = files[700]
load(fn)

coef_names = paste0("coef_", c("cod", "agbm"))
feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

ll_fe = lapply(feat_effects, function (df) {
  df %>%
    pivot_longer(cols = c(coef_names, "truth"), names_to = "optimizer", values_to = "effect") %>%
    group_by(optimizer) %>%
    mutate(y = effect - mean(effect)) %>%
    arrange(optimizer, x)
})
df_fe = do.call(rbind, ll_fe)

feat_id = as.integer(gsub("\\D", "", df_fe$bl))
feat = paste0("Feature ", feat_id)
df_fe$feat = factor(feat, levels = paste0("Feature ", sort(unique(feat_id))))

df_fe$line = df_fe$optimizer
df_fe$line[df_fe$line == "truth"] = "Truth"
df_fe$line[df_fe$line == "coef_cod"] = "COD"
df_fe$line[df_fe$line == "coef_agbm"] = "AGBM"


ggplot(df_fe, aes(x = x, y = y, color = line)) +
  geom_line() +
  #scale_color_viridis(discrete = TRUE) +
  theme_minimal(base_family = "Gyre Bonum") +
  scale_color_brewer(palette = "Set1") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale)
  ) +
  xlab("Feature Value") +
  ylab("Estimated Feature Effect") +
  labs(color = "") +
  facet_wrap(. ~ feat, scales = "free") +
  ggtitle(paste0("Rows: ", bm_extract$config$n, " SNR: ", bm_extract$config$sn_ratio))





coef_names = paste0("coef_", c("cod", "agbm"))
feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

ll_imse = lapply(feat_effects, function (df) {
  df %>%
    pivot_longer(cols = coef_names, names_to = "optimizer", values_to = "effect") %>%
    group_by(optimizer) %>%
    mutate(y = effect - mean(effect), truth = truth - mean(truth)) %>%
    arrange(optimizer, x)
})
df_imse = do.call(rbind, ll_imse)

df_imse_agg = df_imse %>%
  group_by(bl, optimizer) %>%
  summarize(
    mse = mean((truth - y)^2),
    imse = getFeatureIME(x = x, truth = truth, pred = y),
    mae = mean(abs(truth - y)),
    imae = getFeatureIME(x = x, truth = truth, pred = y, loss = function (f,y) abs(f - y))
  ) %>%
  group_by(optimizer) %>%
  summarize(
    mmse = mean(mse),
    mimse = mean(imse),
    mmae = mean(mae),
    mimae = mean(imae)
  )

df_imse_agg

files = list.files("agbm-mom", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  cat("Read - ", k, "/", length(files), "\n")

  load(fn)

  df_temp = data.frame(
    file        = fn,
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    iterations  = c(which.min(bm_extract$log_cod$oob), which.min(bm_extract$log_agbm$oob)),
    min_oob     = c(min(bm_extract$log_cod$oob), min(bm_extract$log_agbm$oob)),
    mom         = bm_extract$momentum,
    optimizer   = c("COD", "AGBM")
  )

  coef_names = paste0("coef_", c("cod", "agbm"))
  feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

  ll_imse = lapply(feat_effects, function (df) {
    df %>%
      pivot_longer(cols = coef_names, names_to = "optimizer", values_to = "effect") %>%
      group_by(optimizer) %>%
      mutate(y = effect - mean(effect), truth = truth - mean(truth)) %>%
      arrange(optimizer, x)
  })
  df_imse = do.call(rbind, ll_imse)

  df_imse_agg = df_imse %>%
    group_by(bl, optimizer) %>%
    summarize(
      mse = mean((truth - y)^2),
      imse = getFeatureIME(x = x, truth = truth, pred = y),
      mae = mean(abs(truth - y)),
      imae = getFeatureIME(x = x, truth = truth, pred = y, loss = function (f,y) abs(f - y))
    ) %>%
    group_by(optimizer) %>%
    summarize(
      mmse = mean(mse, na.rm = TRUE),
      mimse = mean(imse, na.rm = TRUE),
      mmae = mean(mae, na.rm = TRUE),
      mimae = mean(imae, na.rm = TRUE)
    ) %>%
    mutate(optimizer = ifelse(optimizer == "coef_cod", "COD", "AGBM"))

  ll_rows[[k]] = df_temp %>%
    left_join(df_imse_agg, by = "optimizer")

  k = k + 1
}
df_imse = do.call(rbind, ll_rows)
df_imse

df_imse$ptotal = df_imse$ncols + df_imse$ncolsnoise
ggplot(df_imse, aes(x = as.factor(sn_ratio), y = mimse, color = optimizer)) +
  geom_violin() +
  facet_grid(nrows ~ ncols + ncolsnoise)





# Load local files:
# -----------------

files = list.files("performance")
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

#for (fn in files) {
  #load(paste0("performance/", fn))
  #e = try(expr = {
    #bl_mses = getBLMSE(bm_extract)
    #data.frame(
      #file        = fn,
      #date        = bm_extract$date,
      #data_seed   = bm_extract$data_seed,
      #nrows       = bm_extract$config$n,
      #ncols       = bm_extract$config$p,
      #sn_ratio    = bm_extract$config$sn_ratio,
      #rep         = bm_extract$config$rep,
      #ncolsnoise  = bm_extract$config$pnoise,
      #time_init   = c(bm_extract$time_nobinning["init.elapsed"], bm_extract$time_binning["init.elapsed"]),
      #time_fit   = c(bm_extract$time_nobinning["fit.elapsed"], bm_extract$time_binning["fit.elapsed"]),
      #method      = c("nobinning", "binning"),
      #bl_mse      = unlist(bl_mses)
    #)}, silent = TRUE
  #)

  #if (class(e) == "try-error") cat(paste0(k, " - ", fn, ": ", e)) else ll_rows[[k]] = e
  #k = k+1
#}
load("ll_rows_performance.Rda")
df_binning_performance = do.call("rbind", ll_rows)

# Boxplot of base-learner mses:
# -----------------------------


p = ggboxplot(df_binning_performance, x = "nrows", y = "bl_mse",
  color = "method", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  add = "jitter", shape = "method")

my_comparisons = list( c("nobinning", "binning"))
p = p + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)                   # Add global p-value

facet(p, facet.by = c("ncolsnoise", "ncols"),
      short.panel.labs = FALSE)



# Visualize one base-learner of a specific setting:
# -------------------------------------------------

# Here the setting 100:
fn = files[100]
load(paste0("performance/", fn))

# choose base-learner for visualization:
bl_tab = table(bm_extract$trace_binning)
bl = "x9_spline"

coefs_binning = bm_extract$coef_binning[[bl]]
coefs_nobinning = bm_extract$coef_nobinning[[bl]]

mean((coefs_binning - coefs_nobinning)^2)

# Plot estimated effect vs truth:
df_plot = getFeatEffectData(bm_extract,bl)
ggplot(data = df_plot, aes(x = x, y = y, color = method)) + geom_line()

# Oob risk of binning vs nobinning:
df_risk = getOobRiskData(bm_extract)
ggplot(data = df_risk, aes(x = iter, y = risk, color = method, linetype = method)) + geom_line()


# Plot estimated effect over the 20 replications:
fn_pre = "xxx-n50000-p50-pnoise50-snr1-"
files_fix = grep(fn_pre, files)
bl = "x7_spline"

load(paste0("performance/", files[files_fix[1]]))
df_base = getFeatEffectData(bm_extract, bl, TRUE)

set.seed(bm_extract$data_seed * bm_extract$config$rep)
df_base$ynoise = rnorm(n = bm_extract$config$n, mean = df_base$y, sd = sd(df_base$y) / bm_extract$config$sn_ratio)

ll_effect = list()
k = 1
for (fn in files_fix) {
  load(paste0("performance/", files[fn]))
  ll_effect[[k]] = getFeatEffectData(bm_extract, bl, FALSE)
  ll_effect[[k]]$rep = k
  k = k + 1
}

gg = ggplot() +
  geom_line(data = df_base %>% filter(method == "truth"), aes(x = x, y = y), color = "dark red")

for (efct in ll_effect) {
  gg = gg + geom_line(data = efct, aes(x = x, y = y, color = method), alpha = 0.2)
}
gg + scale_color_viridis(discrete = TRUE)





