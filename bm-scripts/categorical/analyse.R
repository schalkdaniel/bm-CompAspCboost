library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

source("../../R/bm-sim-data.R")
source("helper.R")

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

mem_setup = 50

k = 1
for (fn in files) {
  load(paste0("memory/", fn))
  if(is.null(bm_extract$ms_extract_ridge)) {
    cat(fn, " does not have ridge memory heap size\n")
  } else {
    ll_rows[[k]] = data.frame(
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      nclasses    = bm_extract$cls_config["ncls"][1,1],
      nnoninfocls = bm_extract$cls_config["nic"][1,1],
      #rep         = bm_extract$config$rep,    # rep is always 1 for memory
      ncolsnoise  = bm_extract$config$pnoise,
      mem         = c(last(bm_extract$ms_extract_linear$mem_heap_B), last(bm_extract$ms_extract_binary$mem_heap_B), last(bm_extract$ms_extract_ridge$mem_heap_B)) - mem_setup,
      unit        = c(last(bm_extract$ms_extract_linear$unit), last(bm_extract$ms_extract_binary$unit), last(bm_extract$ms_extract_ridge$unit)),
      method      = c("linear", "binary", "ridge")
    )
  }
  k = k+1
}
df_cat_memory = do.call("rbind", ll_rows)

# Plot real memory as lines:
# --------------------------

## NOT RELEVANT!!!

#gg = df_cat_memory %>%
#df_cat_memory %>%
  #group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  #summarize(mem = median(mem)) %>%
  #ggplot(aes(x = nrows, y = mem / 1024, color = method, linetype = as.factor(nclasses))) +
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
    #scale_x_continuous(breaks = sort(unique(df_cat_memory$nrows))[-c(1,2)]) +
    #scale_color_viridis(discrete = TRUE) +
    #xlab("Number of Rows") +
    #ylab("Allocated Memory in GB") +
    #labs(color = "") +
    #facet_grid(ncolsnoise ~ ncols, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "memory_lines.pdf", width = dinA4width * 2/3 * 0.6, height = dinA4width * 2/3, units = "mm")


# Plot used memory (proportional):
# --------------------------------

df_plt_mem = df_cat_memory %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  summarize(mem = median(mem)) %>%
  group_by(nrows, ncols, ncolsnoise, nclasses) %>%
  summarize(rel_mem_bin = mem[method == "linear"] / mem[method == "binary"], rel_mem_ridge = mem[method == "linear"] / mem[method == "ridge"], ptotal = ncols[1] + ncolsnoise[1]) %>%
  gather(key = "method", value = "rel_mem", starts_with("rel_mem"))

df_plt_mem$ptotal = factor(df_plt_mem$ptotal, levels = as.character(sort(unique(df_plt_mem$ptotal))))
df_plt_mem$method[df_plt_mem$method == "rel_mem_bin"] = "Binary"
df_plt_mem$method[df_plt_mem$method == "rel_mem_ridge"] = "Ridge"

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
  labs(color = "Number of\nFeatures") +
  facet_grid(factor(paste0(nclasses, " classes"), levels = paste0(sort(unique(nclasses)), " classes")) ~ method, scales= "free_y")
  #facet_grid(method ~ factor(paste0(nclasses, " classes"), levels = paste0(sort(unique(nclasses)), " classes")))
  #facet_grid(method ~ nclasses, scales = "free_y")

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
  ncls = as.integer(strsplit(x = strsplit(x = fn, split = "nclasses")[[1]][2], split = "-informative")[[1]][1])
  nic = as.integer(strsplit(x = strsplit(x = fn, split = "informative-classes")[[1]][2], split = ".Rda")[[1]][1])
  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    nclasses    = ncls,
    nnoninfocls = bm_extract$config_classes["nic"][1,1],
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    time        = c(sum(bm_extract$time_linear), sum(bm_extract$time_binary), sum(bm_extract$time_ridge)),
    method      = c("linear", "binary", "ridge")
  )
  k = k+1
}
df_cat_runtime = do.call("rbind", ll_rows)

df_plt_run = df_cat_runtime %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, nclasses) %>%
  summarize(
    rel_time_binary = time[method == "linear"] / time[method == "binary"],
    rel_time_ridge = time[method == "linear"] / time[method == "ridge"],
    ptotal = ncols[1] + ncolsnoise[1]
  ) %>%
  gather(key = "method", value = "rel_time", starts_with("rel_time"))

df_plt_run$ptotal = factor(df_plt_run$ptotal, levels = as.character(sort(unique(df_plt_run$ptotal))))
df_plt_run$method[df_plt_run$method == "rel_time_binary"] = "Binary"
df_plt_run$method[df_plt_run$method == "rel_time_ridge"] = "Ridge"

gg = ggplot(data = df_plt_run, aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal))) +
  geom_hline(yintercept = 1, lty = 2, col = "dark red") +
  geom_violin(alpha = 0.5) +
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
  facet_grid(factor(paste0(nclasses, " classes"), levels = paste0(sort(unique(nclasses)), " classes")) ~ method )
  #facet_grid(method ~ factor(paste0(nclasses, " classes"), levels = paste0(sort(unique(nclasses)), " classes")))
  #facet_grid(method ~ nclassess, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "runtime_rel_violines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")




## performance
## ----------------------------------------------

files = list.files("performance")
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  load(paste0("performance/", fn))
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
    noise_
    method      = c("nobinning", "binning")
  )
  k = k+1
}
df_binning_runtime = do.call("rbind", ll_rows)

fn = files[100]
load(paste0("performance/", fn))

bl_tab = table(bm_extract$trace_binning)
bl = "x9_spline"

coefs_binning = bm_extract$coef_binning[[bl]]
coefs_nobinning = bm_extract$coef_nobinning[[bl]]

mse_coefs = mean((coefs_binning - coefs_nobinning)^2)

df_plot = getFeatEffectData(bm_extract, "x9_spline")
ggplot(data = df_plot, aes(x = x, y = y, color = method)) + geom_line()

df_risk = getOobRiskData(bm_extract)
ggplot(data = df_risk, aes(x = iter, y = risk, color = method, linetype = method)) + geom_line()




fn_pre = "xxx-n50000-p50-pnoise50-snr0.1-"
files_fix = grep(fn_pre, files)
bl = "x20_spline"

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

gg = ggplot()
gg = gg +
  #geom_point(data = df_base %>% filter(method == "truth"), aes(x = x, y = ynoise), alpha = 0.1, stroke = 0) +
  geom_line(data = df_base %>% filter(method == "truth"), aes(x = x, y = y), color = "dark red")

for (efct in ll_effect) {
  gg = gg + geom_line(data = efct, aes(x = x, y = y, color = method), alpha = 0.2)
}
gg + scale_color_viridis(discrete = TRUE)





