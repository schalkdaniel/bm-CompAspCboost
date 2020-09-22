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
  if (grepl("binning", fn)) {
    ll_rows[[k]] = data.frame(
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      #rep         = bm_extract$config$rep,    # rep is always 1 for memory
      ncolsnoise  = bm_extract$config$pnoise,
      mem         = last(bm_extract$ms_extract_nobinning$mem_heap_B) - mem_setup,
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
      mem         = last(bm_extract$ms_extract_agbm$mem_heap_B) - mem_setup,
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
    axis.title = element_text(size = 10 * font_scale)
  ) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of Rows") +
  ylab("Relative Allocated Memory") +
  labs(color = "Number of\nFeatures") #+
  #facet_grid(. ~ ncols, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "memory_rel_lines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")



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
    axis.title = element_text(size = 10 * font_scale)
  ) +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE) +
  xlab("Number of Rows") +
  ylab("Relative Speedup") +
  labs(color = "Number of\nFeatures", fill = "Number of\nFeatures") +
  facet_grid(. ~ phase, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "runtime_rel_violines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")


## performance
## ----------------------------------------------

# Result from finding a good momentum parameter:
# ----------------------------------------------


files = list.files("agbm-mom", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  load(fn)
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
    iterations  = c(length(bm_extract$log_cod$oob), length(bm_extract$log_agbm$oob)),
    min_oob     = c(min(bm_extract$log_cod$oob), min(bm_extract$log_agbm$oob)),
    mom         = bm_extract$momentum
  )
  k = k+1
}
df_agbm = do.call("rbind", ll_rows)
#df_agbm$time = df_agbm$time_init + df_agbm$time_fit

df_plt = df_agbm %>%
  mutate(time = time_init + time_fit) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, mom) %>%
  summarize(
    #rel_time_init = time_init[method == "COD"] / time_init[method == "AGBM"],
    #rel_time_fit = time_fit[method == "COD"] / time_fit[method == "AGBM"],
    rel_time = time[method == "COD"] / time[method == "AGBM"],
    ptotal = ncols[1] + ncolsnoise[1],
    diffiter = iterations[method == "COD"] - iterations[method == "AGBM"],
    diffoob  = min_oob[method == "COD"] - min_oob[method == "AGBM"]
  )

mod_iter = lm(diffiter ~ mom + nrows + ncols + ncolsnoise + as.factor(sn_ratio), data = df_plt)
summary(mod_iter)
mod_oob = lm(diffoob ~ mom + nrows + ncols + ncolsnoise  + as.factor(sn_ratio), data = df_plt)
summary(mod_oob)

df_plt_agbm = df_plt %>%
  group_by(mom) %>%
  summarize(
    m_diffoob = mean(diffoob), sd_diffoob = sd(diffoob), min_diffoob = min(diffoob), max_diffoob = max(diffoob),
    m_diffiter = mean(diffiter), sd_diffiter = sd(diffiter), min_diffiter = min(diffiter), max_diffiter = max(diffiter)
  )

df_plt_run$phase[df_plt_run$phase == "rel_time"] = "Initialization + Fitting"
df_plt_run$phase[df_plt_run$phase == "rel_time_init"] = "Initialization"
df_plt_run$phase[df_plt_run$phase == "rel_time_fit"] = "Fitting"
df_plt_run$phase = factor(df_plt_run$phase, levels = c("Initialization + Fitting", "Initialization", "Fitting"))


ggplot(data = df_plt_run, aes(x = m_diffoob, y = m_diffiter, color = mom)) +
  geom_point()




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






