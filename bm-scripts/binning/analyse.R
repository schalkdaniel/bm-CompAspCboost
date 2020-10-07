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

files = list.files("memory")
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

mem_setup = 50

for (fn in files) {
  load(paste0("memory/", fn))

  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)
  dat_noise = dat$data

  set.seed(bm_extract$data_seed * bm_extract$config$rep)
  dat_noise$y = rnorm(n = bm_extract$config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / bm_extract$config$sn_ratio)

  mem_rsim = sum(c(object.size(dat), object.size(dat_noise))) / 1024^2

  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    #rep         = bm_extract$config$rep,    # rep is always 1 for memory
    ncolsnoise  = bm_extract$config$pnoise,
    mem         = c(last(bm_extract$ms_extract_nobinning$mem_heap_B), last(bm_extract$ms_extract_binning$mem_heap_B)) - mem_setup - mem_rsim,
    unit        = c(last(bm_extract$ms_extract_nobinning$unit), last(bm_extract$ms_extract_binning$unit)),
    method      = c("no binning", "binning")
  )
  k = k+1
}
df_binning_memory = do.call("rbind", ll_rows)


memSave = function (n, dspline, p, b, n_bl = 1, mem_const = 0, add_fix = FALSE) {

  n_knots = p + dspline - 1

  mem_pen = p^2 * 8 + (n_knots + 2) * 8
  mem_nob = n * (dspline + 1) * 12 + (p + 3) * 4 + mem_pen
  #mem_nob = n * (dspline + 1) * 12 + (p + 3) * 4 + mem_pen
  mem_bin = n^(1/b) * (dspline + 1) * 12 + n * 4 + (p + 3) * 4 + mem_pen

  if (add_fix) {
    mem_fix_nob = n_bl * p * 8 + n_bl * n * 8 + 4 * n * 8 + n_bl * p * 8 + n_bl * p^2 * 8
    mem_fix_bin = n_bl * p * 8 + n_bl * n * 8 + 4 * n * 8 + n_bl * p * 8 + n_bl * p^2 * 8
  } else {
    mem_fix_nob = mem_fix_bin = 0
  }

  mem_nob = (n_bl * mem_nob + mem_fix_nob + mem_const) / 1024^2
  mem_bin = (n_bl * mem_bin + mem_fix_bin + mem_const) / 1024^2

  rel = mem_nob / mem_bin
  return (c(rel = rel, mem_nob = mem_nob, mem_bin = mem_bin))
}
memSave(n = 100000, dspline = 3, p = 24, b = 2, n_bl = 300)

m = memSave(n = 100000, dspline = 3, p = 24, b = 2, n_bl = 300, add_fix = TRUE)
m
1869.13 - m["mem_nob"]
500.54 - m["mem_bin"]

m = memSave(n = 50000, dspline = 3, p = 24, b = 2, n_bl = 100, add_fix = TRUE)
m
952.24 - m["mem_nob"]
268.54 - m["mem_bin"]


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

df_plt_mem = df_binning_memory %>%
  #filter(ncolsnoise == 10) %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(mem)) %>%
  group_by(nrows, ncolsnoise, ncols) %>%
  summarize(rel_mem = mean_mem[method == "no binning"] / mean_mem[method == "binning"], ptotal = ncols[1] + ncolsnoise[1])

df_plt_mem$ptotal = factor(df_plt_mem$ptotal, levels = as.character(sort(unique(df_plt_mem$ptotal))))

gg = ggplot(data = df_plt_mem, aes(x = nrows, y = rel_mem, color = ptotal, group = paste0(ncols, ncolsnoise))) +
  geom_hline(yintercept = 1, col = "dark red", lty = 2) +
  geom_line() +
  geom_point(size = 4) +
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
  scale_x_continuous(breaks = sort(unique(df_binning_memory$nrows))) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of Rows") +
  ylab("Relative Allocated Memory") +
  labs(color = "Number of\nFeatures") #+
  #facet_grid(. ~ ncols, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "memory_rel_lines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.7, units = "mm")

tmp = df_binning_memory %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(mem)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise, nobinning = `no binning`) %>%
  group_by(nrows, ptotal) %>%
  select(nrows, ptotal, nobinning, binning) %>%
  filter(nobinning == max(nobinning)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal) %>%
  mutate(rel = nobinning / binning)

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
    time_init   = c(bm_extract$time_nobinning["init.elapsed"], bm_extract$time_binning["init.elapsed"]),
    time_fit   = c(bm_extract$time_nobinning["fit.elapsed"], bm_extract$time_binning["fit.elapsed"]),
    method      = c("nobinning", "binning")
  )
  k = k+1
}
df_binning_runtime = do.call("rbind", ll_rows)


df_plt_run = df_binning_runtime %>%
  mutate(time = time_init + time_fit) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise) %>%
  summarize(
    rel_time_init = time_init[method == "nobinning"] / time_init[method == "binning"],
    rel_time_fit = time_fit[method == "nobinning"] / time_fit[method == "binning"],
    rel_time = time[method == "nobinning"] / time[method == "binning"],
    ptotal = ncols[1] + ncolsnoise[1]
  ) %>%
  gather(key = "phase", value = "rel_time", starts_with("rel_time"))

df_plt_run$phase[df_plt_run$phase == "rel_time"] = "Initialization + Fitting"
df_plt_run$phase[df_plt_run$phase == "rel_time_init"] = "Initialization"
df_plt_run$phase[df_plt_run$phase == "rel_time_fit"] = "Fitting"
df_plt_run$phase = factor(df_plt_run$phase, levels = c("Initialization + Fitting", "Initialization", "Fitting"))

df_plt_run$ptotal = factor(df_plt_run$ptotal, levels = as.character(sort(unique(df_plt_run$ptotal))))


gg = ggplot(data = df_plt_run %>% filter(rel_time < 10, rel_time > 1), aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal))) +
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
#ggsave(plot = gg, filename = "runtime_rel_violines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")


tmp = df_binning_runtime %>%
  mutate(time = time_init + time_fit) %>%
  filter(rep == 1) %>%
  select(nrows, ncols, ncolsnoise, sn_ratio, time, method) %>%
  pivot_wider(names_from = method, values_from = time) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  select(nrows, ptotal, nobinning, binning) %>%
  group_by(nrows, ptotal) %>%
  filter(nobinning == max(nobinning)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal) %>%
  mutate(rel = nobinning / binning)

tmp$nobinning = tmp$nobinning / 60
tmp$binning = tmp$binning / 60
knitr::kable(round(tmp, 2), format = "latex")


## performance
## ----------------------------------------------

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
















# Performance: Visualize one setting:
files = list.files("performance", full.names = TRUE)
files = files[grep("xxx", files)]

fn = "performance/xxx-n10000-p10-pnoise20-snr0.1-rep3-df5-binroot2.Rda"
fn = "performance/xxx-n50000-p10-pnoise20-snr0.1-rep3-df9-binroot2.Rda"

fn = files[100]
load(fn)

coef_names = paste0("coef_", c("binning", "nobinning"))
feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

ll_fe = lapply(feat_effects, function (df) {
  df %>%
    pivot_longer(cols = all_of(c(coef_names, "truth")), names_to = "method", values_to = "effect") %>%
    group_by(method) %>%
    mutate(y = effect - mean(effect)) %>%
    arrange(method, x)
})
df_fe = do.call(rbind, ll_fe)

feat_id = as.integer(gsub("\\D", "", df_fe$bl))
feat = paste0("Feature ", feat_id)
df_fe$feat = factor(feat, levels = paste0("Feature ", sort(unique(feat_id))))

df_fe$line = df_fe$method
df_fe$line[df_fe$line == "truth"] = "Truth"
df_fe$line[df_fe$line == "coef_binning"] = "Binning"
df_fe$line[df_fe$line == "coef_nobinning"] = "No Binning"


gg = ggplot(df_fe, aes(x = x, y = y, color = line)) +
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
  scale_x_continuous(breaks = NULL) +
  facet_wrap(. ~ feat, scales = "free") +
  ggtitle(paste0("Rows: ", bm_extract$config$n, " SNR: ", bm_extract$config$sn_ratio))

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "performance_effects1.pdf", width = dinA4width, height = dinA4width, units = "mm")








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









files = list.files("performance", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  cat("Read - ", k, "/", length(files), "\n")

  load(fn)

  if (! is.null(bm_extract$trace_binning)) {
    df_temp = data.frame(
      file        = fn,
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      rep         = bm_extract$config$rep,
      ncolsnoise  = bm_extract$config$pnoise,
      df          = bm_extract$df,
      bin_root    = bm_extract$bin_root,
      iterations  = c(which.min(bm_extract$log_binning$oob), which.min(bm_extract$log_nobinning$oob_risk)),
      min_oob     = c(min(bm_extract$log_binning$oob), min(bm_extract$log_nobinning$oob_risk)),
      method = c("Binning", "No Binning")
    )

    coef_names = paste0("coef_", c("binning", "nobinning"))
    feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

    ll_imse = lapply(feat_effects, function (df) {
      df %>%
        pivot_longer(cols = coef_names, names_to = "method", values_to = "effect") %>%
        group_by(method) %>%
        mutate(y = effect - mean(effect), truth = truth - mean(truth)) %>%
        arrange(method, x)
    })
    df_imse = do.call(rbind, ll_imse)

    df_imse_agg = df_imse %>%
      group_by(bl, method) %>%
      summarize(
        mse = mean((truth - y)^2),
        imse = getFeatureIME(x = x, truth = truth, pred = y),
        mae = mean(abs(truth - y)),
        imae = getFeatureIME(x = x, truth = truth, pred = y, loss = function (f,y) abs(f - y))
      ) %>%
      group_by(method) %>%
      summarize(
        mmse = mean(mse, na.rm = TRUE),
        mimse = mean(imse, na.rm = TRUE),
        mmae = mean(mae, na.rm = TRUE),
        mimae = mean(imae, na.rm = TRUE)
      ) %>%
      mutate(method = ifelse(method == "coef_binning", "Binning", "No Binning"))

    ll_rows[[k]] = df_temp %>%
      left_join(df_imse_agg, by = "method")

    k = k + 1
  }
}
load("ll_rows_performance_measures.Rda")
df_imse = do.call(rbind, ll_rows)
df_imse

df_imse = df_imse %>%
  mutate(
    method_n = ifelse(method == "Binning", paste0(method, " ", bin_root), method),
    ptotal = ncols + ncolsnoise
  )

gg = df_imse %>%
  filter(bin_root != 9) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, df) %>%
  summarize(
    rel_bin2 = mimse[method_n == "No Binning"][1] / mimse[method_n == "Binning 2"],
    rel_bin4 = mimse[method_n == "No Binning"][1] / mimse[method_n == "Binning 4"]
  ) %>%
  pivot_longer(cols = starts_with("rel_bin"), names_to = "bin_root", values_to = "rel") %>%
  mutate(method_n = ifelse(bin_root == "rel_bin2", "2", "4")) %>%

  ggplot(aes(x = as.factor(nrows), y = rel, fill = method_n, color = method_n)) +
    geom_hline(yintercept = 1, color = "dark red", linetype = "dashed") +
    geom_boxplot(alpha = 0.5) +
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
    #scale_color_viridis(discrete = TRUE) +
    #scale_fill_viridis(discrete = TRUE) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    xlab("Number of Rows") +
    ylab("Relative Deviation of the\nMean Integrated Squared Error\nCompared No Binning") +
    labs(color = "Binning Root", fill = "Binning Root") +
    facet_grid(paste0("SNR = ", sn_ratio) ~ paste0("df = ", df), scales = "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "performance_boxplots_rel.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.6, units = "mm")









