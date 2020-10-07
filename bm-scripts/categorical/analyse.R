library(dplyr)
library(ggplot2)
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

files = list.files("memory", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()

mem_setup = 50

k = 1
for (fn in files) {
  load(fn)

  set.seed(bm_extract$data_seed)
  dat = simCategoricalData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise, nclasses = bm_extract$cls_config$ncls[1], ncnoise = bm_extract$cls_config$nic[1])

  cnames = colnames(dat$data)
  for (fn in cnames[cnames != "y"]) {
    dat$data[[fn]] = as.character(dat$data[[fn]])
  }
  dat_noise = dat$data

  mem_rsim = sum(c(object.size(dat), object.size(dat_noise))) / 1024^2

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
      mem         = c(last(bm_extract$ms_extract_linear$mem_heap_B), last(bm_extract$ms_extract_binary$mem_heap_B), last(bm_extract$ms_extract_ridge$mem_heap_B)) - mem_setup - mem_rsim,
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
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale)
  ) +
  scale_x_continuous(breaks = sort(unique(df_cat_memory$nrows))) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of Rows") +
  ylab("Relative Allocated Memory") +
  labs(color = "Number of\nFeatures") +
  facet_grid(factor(paste0(nclasses, " classes"), levels = paste0(sort(unique(nclasses)), " classes")) ~ method, scales= "free_y")
  #facet_grid(method ~ factor(paste0(nclasses, " classes"), levels = paste0(sort(unique(nclasses)), " classes")))
  #facet_grid(method ~ nclasses, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "categorical_memory_rel_lines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")


tmp = df_cat_memory %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  summarize(mean_mem = median(mem)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method, nclasses) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  group_by(nrows, ptotal, nclasses) %>%
  select(nrows, ptotal, linear, binary, ridge, nclasses) %>%
  filter(linear == max(linear), nclasses %in% c(5, 20)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal, nclasses) #%>%
  #mutate(rel = nobinning / binning)

knitr::kable(round(tmp, 2), format = "latex")




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
  facet_grid(factor(paste0(nclasses, " classes"), levels = paste0(sort(unique(nclasses)), " classes")) ~ method, scales = "free_y")
  #facet_grid(method ~ factor(paste0(nclasses, " classes"), levels = paste0(sort(unique(nclasses)), " classes")))
  #facet_grid(method ~ nclassess, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "categorical_runtime_rel_violines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")



tmp = df_cat_runtime %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  summarize(mean_mem = median(time)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method, nclasses) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  group_by(nrows, ptotal, nclasses) %>%
  select(nrows, ptotal, linear, binary, ridge, nclasses) %>%
  filter(linear == max(linear), nclasses %in% c(5, 20)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal, nclasses) #%>%
  #mutate(rel = nobinning / binning)

tmp$binary = tmp$binary / 60
tmp$linear = tmp$linear / 60
tmp$ridge  = tmp$ridge / 60
knitr::kable(round(tmp, 2), format = "latex")


## performance
## ----------------------------------------------

files = list.files("performance", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

#for (fn in sample(x = files, size = 2000, replace = FALSE)) {
#for (fn in files[1:10000]) {
#for (fn in files[10001:20000]) {
for (fn in files[20001:length(files)]) {
  cat(as.character(Sys.time()), "Read: ", k , "/", length(files), "\n")

  load(fn)

  selected_feat_bin = unlist(lapply(strsplit(x = bm_extract$trace_binary, split = "_"), function (x) x[1]))
  selected_feat_ridge = unlist(lapply(strsplit(x = bm_extract$trace_ridge, split = "_"), function (x) x[1]))
  ncls = as.integer(strsplit(x = strsplit(x = fn, split = "nclasses")[[1]][2], split = "-informative")[[1]][1])

  n_noise_bin = sum(grepl(x = selected_feat_bin, pattern = "noise"))
  n_feat_bin = length(selected_feat_bin) - n_noise_bin

  n_noise_ridge = sum(grepl(x = selected_feat_ridge, pattern = "noise"))
  n_feat_ridge = length(selected_feat_ridge) - n_noise_ridge

  set.seed(bm_extract$data_seed)
  dat = simCategoricalData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise, nclasses = bm_extract$config_classes$ncls[1], ncnoise = bm_extract$config_classes$nic[1])
  oob_int = mean((mean(dat$data$y) - dat$data$y)^2)

  #ll_rows[[k]] = data.frame(

    #date        = bm_extract$date,
    #data_seed   = bm_extract$data_seed,
    #nrows       = bm_extract$config$n,
    #ncols       = bm_extract$config$p,
    #sn_ratio    = bm_extract$config$sn_ratio,
    #rep         = bm_extract$config$rep,
    #ncolsnoise  = bm_extract$config$pnoise,
    #nclasses    = ncls,
    #nnoninfocls = bm_extract$config_classes["nic"][1,1],
    #time_init   = rep(c(bm_extract$time_binary["init.elapsed"], bm_extract$time_ridge["init.elapsed"]), times = c(length(selected_feat_bin), length(selected_feat_ridge))),
    #time_fit   = rep(c(bm_extract$time_binary["fit.elapsed"], bm_extract$time_ridge["fit.elapsed"]), times = c(length(selected_feat_bin), length(selected_feat_ridge))),
    #method      = rep(c("binary", "ridge"), times = c(length(selected_feat_bin), length(selected_feat_ridge))),
    #selected_feat = c(selected_feat_bin, selected_feat_ridge),
    #oob_risk_int = rep(c(bm_extract$log_binary$logger_data[1,2], bm_extract$log_ridge$logger_data[1,2]), times = c(length(selected_feat_bin), length(selected_feat_ridge))),
    #oob_risk_min = rep(c(min(bm_extract$log_binary$logger_data[1,2]), min(bm_extract$log_ridge$logger_data[,2])), times = c(length(selected_feat_bin), length(selected_feat_ridge)))
  #)
  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    nclasses    = ncls,
    nnoninfocls = bm_extract$config_classes["nic"][1,1],
    time_init   = c(bm_extract$time_binary["init.elapsed"], bm_extract$time_ridge["init.elapsed"]),
    time_fit   = c(bm_extract$time_binary["fit.elapsed"], bm_extract$time_ridge["fit.elapsed"]),
    method      = c("binary", "ridge"),
    iterations = c(length(selected_feat_bin), length(selected_feat_ridge)),
    oob_risk_int = oob_int,
    oob_risk_min = c(min(bm_extract$log_binary$logger_data[,2]), min(bm_extract$log_ridge$logger_data[,2])),
    n_selected   = c(n_feat_bin, n_feat_ridge),
    n_noise      = c(n_noise_bin, n_noise_ridge)
  )
  k = k+1
}
save(ll_rows, file = "ll_rows_cat.Rda")
load("ll_rows_cat.Rda")


#ll_n = lapply(ll_rows, function (x) {
  #tbin   = table(x$selected_feat[x$method == "binary"])
  #tridge = table(x$selected_feat[x$method == "ridge"])

  #nnoise_bin = sum(grepl("noise", names(tbin)))
  #nfeat_bin = sum(grepl("xx", names(tbin)))
  #nnoise_ridge = sum(grepl("noise", names(tridge)))
  #nfeat_ridge = sum(grepl("xx", names(tridge)))

  #data.frame(
    #nrows = x$nrows[1],
    #ncols = x$ncols[1],
    #sn_ratio = x$sn_ratio[1],
    #rep = x$rep[1],
    #ncolsnoise = x$ncolsnoise[1],
    #nclasses = x$nclasses[1],
    #nnoninfocls = x$nnoninfocls[1],
    #method = c("binary", "binary", "ridge", "ridge"),
    #ftype = c("feat", "noise", "feat", "noise"),
    #nselect = c(nfeat_bin, nnoise_bin, nfeat_ridge, nnoise_ridge),
    #nreal = c(x$ncols[1], x$ncolsnoise[1], x$ncols[1], x$ncolsnoise[1])
  #)
#})

#df_cat_n = do.call("rbind", ll_n)
#df_cat_n$rates = df_cat_n$nselect / df_cat_n$nreal

#ggplot(data = df_cat_n, aes(x = nrows, y = rates, color = method, shape = ftype)) +
  #geom_point() +
  #facet_grid(ncols ~ nclasses)

df_cat = do.call("rbind", ll_rows)


gg_sel = df_cat %>%
  pivot_longer(names_to = "feat_type", values_to = "selected", cols = starts_with("n_")) %>%
  mutate(
    rel_selected = selected / iterations,
    ft = ifelse(feat_type == "n_selected", "Informative", "Noise")
  ) %>%
  #group_by(nrows, ncols, ncolsnoise, ft, method, sn_ratio, nclasses) %>%
  group_by(nrows, ncols, ncolsnoise, ft, method, sn_ratio) %>%
  filter(ft == "Informative") %>%
  #summarize(rel = median(rel_selected), min_rel = min(rel_selected), max_rel = max(rel_selected)) %>%
  summarize(
    rel = median(rel_selected),
    min_rel = median(rel_selected) - sd(rel_selected),
    max_rel = median(rel_selected) + sd(rel_selected),
    pn_rel = ncolsnoise[1] / ncols[1]
  ) %>%
  mutate(pn_rel = ifelse(pn_rel == 0.4, 0.5, pn_rel)) %>%
  #filter(ncols == 50, nclasses == 20) %>%
    ggplot(aes(x = nrows, y = rel, linetype = method, color = as.factor(sn_ratio))) +
      geom_linerange(aes(ymin = min_rel, ymax = max_rel), alpha = 0.5, position = position_dodge(width = 0.05)) +
      geom_line(position = position_dodge(width = 0.05)) +
      #scale_fill_viridis(discrete = TRUE) +
      #scale_color_viridis(discrete = TRUE) +
      scale_color_brewer(palette = "Set1") +
      xlab("Number of Rows\n(log10 Scale)") +
      ylab("Relative Amount of Selected\nInformative Feature vs. Noise") +
      labs(linetype = "Method", fill = "Method", color = "Signal to Noise\nRatio") +
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
      #scale_x_continuous(breaks = sort(unique(df_cat$nrows)), trans = "log10") +
      scale_x_continuous(trans = "log10") +
      #scale_x_continuous(breaks = NULL) +
      facet_grid(pn_rel  ~ ncols)#, scales = "free_y")
gg_sel



#gg_sel = df_cat %>%
df_cat %>%
  pivot_longer(names_to = "feat_type", values_to = "selected", cols = starts_with("n_")) %>%
  mutate(
    rel_selected = selected / iterations,
    ft = ifelse(feat_type == "n_selected", "Informative", "Noise")
  ) %>%
  filter(ft == "Informative") %>%
  group_by(nrows, ncols, ncolsnoise, rep, ft, sn_ratio, nclasses, nnoninfocls) %>%
  summarize(rel_diff = rel_selected[method == "ridge"] - rel_selected[method == "binary"]) %>%
  group_by(nrows, ncols, ncolsnoise, ft, nclasses, sn_ratio, nnoninfocls) %>%
  #group_by(nrows, ncols, ncolsnoise, ft, sn_ratio, nnoninfocls) %>%
  summarize(
    rel = median(rel_diff),
    min_rel = median(rel_diff) - sd(rel_diff),
    max_rel = median(rel_diff) + sd(rel_diff),
    pn_rel = ncolsnoise[1] / ncols[1],
    cls_rel = nnoninfocls[1] / nclasses[1]
  ) %>%
  mutate(
    pn_rel = ifelse(pn_rel == 0.4, 0.5, pn_rel),
    cls_rel = ifelse(cls_rel == 0.4, 0.5, cls_rel)
  ) %>%
    #ggplot(aes(x = nrows, y = rel, linetype = as.factor(cls_rel), color = as.factor(sn_ratio))) +
    ggplot(aes(x = nrows, y = rel, color = as.factor(sn_ratio))) +
      geom_linerange(aes(ymin = min_rel, ymax = max_rel), alpha = 0.5, position = position_dodge(width = 0.05)) +
      geom_line(position = position_dodge(width = 0.05)) +
      #scale_fill_viridis(discrete = TRUE) +
      #scale_color_viridis(discrete = TRUE) +
      scale_color_brewer(palette = "Set1") +
      xlab("Number of Rows\n(log10 Scale)") +
      ylab("Relative Amount of Selected\nInformative Feature vs. Noise") +
      labs(linetype = "Method", fill = "Method", color = "Signal to Noise\nRatio") +
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
      scale_x_continuous(trans = "log10") +
      facet_grid(pn_rel + cls_rel  ~ ncols)#, scales = "free_y")
#gg_sel

















dinA4width = 210 * font_scale
ggsave(plot = gg_sel, filename = "categorical_selection_full.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.8, units = "mm")

gg_sel = df_cat %>%
  pivot_longer(names_to = "feat_type", values_to = "selected", cols = starts_with("n_")) %>%
  mutate(rel_selected = selected / iterations, ft = ifelse(feat_type == "n_selected", "Informative", "Noise")) %>%
  group_by(nrows, ncols, ncolsnoise, ft, method, sn_ratio, nclasses) %>%
  summarize(rel = median(rel_selected)) %>%
  filter(ncols == 50, nclasses == 20) %>%
    ggplot(aes(x = nrows, y = rel, color = method, linetype = ft)) +
      geom_line() +
      #scale_fill_viridis(discrete = TRUE) +
      #scale_color_viridis(discrete = TRUE) +
      scale_color_brewer(palette = "Set1") +
      xlab("Number of Rows") +
      ylab("Relative Amount of Selected\nInformative Feature vs. Noise") +
      labs(color = "Method", fill = "Method", linetype = "Feature") +
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
      #scale_x_continuous(breaks = sort(unique(df_cat$nrows))) +
      scale_x_continuous(breaks = NULL) +
      facet_grid(nclasses + sn_ratio ~ ncols + ncolsnoise)

dinA4width = 210 * font_scale
ggsave(plot = gg_sel, filename = "categorical_selection_filtered.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.8, units = "mm")






df_plt = df_cat  %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, nclasses, nnoninfocls) %>%
  summarize(
    ptotal = ncols[1] + ncolsnoise[1],
    diffiter = (iterations[method == "binary"] - iterations[method == "ridge"]) / iterations[method == "binary"],
    diffoob  = (oob_risk_min[method == "binary"] - oob_risk_min[method == "ridge"]) / oob_risk_min[method == "binary"],
    diffiter_t = (iterations[method == "binary"] - iterations[method == "ridge"]),
    diffoob_t  = (oob_risk_min[method == "binary"] - oob_risk_min[method == "ridge"]),
    diffoob_int = (oob_risk_int[method == "binary"] - oob_risk_int[method == "ridge"]) / oob_risk_int[method == "binary"],
    diffoob_int_t = oob_risk_int[method == "binary"] - oob_risk_int[method == "ridge"],
    range_cod = oob_risk_int[method == "binary"],
    iter_cod = iterations[method == "binary"],
    range_agbm = oob_risk_int[method == "ridge"]
  )


gg_oob = ggplot() +
  #geom_point(data = df_plt %>% filter(diffiter > -1, iter_cod < 20000, !((sn_ratio == 1) & (diffiter < 0.6)), !((sn_ratio == 10) & (diffiter < 0.75))), aes(x = diffiter, y = range_cod, color = as.factor(ncols)), alpha = 0.3, stroke = 1) +
  #geom_segment(data = df_plt %>% filter(diffiter > -1, iter_cod < 20000, !((sn_ratio == 1) & (diffiter < 0.6)), !((sn_ratio == 10) & (diffiter < 0.75))), aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = as.factor(ncols)), alpha = 1) +
  geom_point(data = df_plt, aes(x = diffiter, y = range_cod, color = as.factor(ncols)), alpha = 0.3) +
  geom_segment(data = df_plt, aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = as.factor(ncols)), alpha = 1) +
  geom_point(data = df_plt %>% filter(iter_cod == 20000, !((sn_ratio == 1) & (diffiter < 0.6)), !((sn_ratio == 10) & (diffiter < 0.75))),
    aes(x = diffiter_t, y = range_agbm, color = as.factor(ncols)), alpha = 0.3, stroke = 1, shape = 4, show.legend = FALSE) +
  #scale_color_viridis(discrete = TRUE)
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_family = "Gyre Bonum") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 9 * font_scale)
  ) +
  xlab("Relative Improvement of Iterations") +
  ylab("Risk Improvement from\nIntercept Model") +
  labs(color = "Number of informative\nfeatures") +
  facet_grid(nrows ~ sn_ratio + nclasses, scales = "free")

dinA4width = 210 * font_scale
ggsave(plot = gg_oob, filename = "categorical_oob.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.8, units = "mm")


df_cat_iter = df_cat %>%
  select(nrows, ncols, sn_ratio, rep, ncolsnoise, nclasses, nnoninfocls, method, iterations) %>%
  pivot_wider(names_from = "method", values_from = "iterations") %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  filter(binary < 20000)

ll_temp = list()
k = 1
for (p in unique(df_cat_iter$ptotal)) {
  for (n in unique(df_cat_iter$nrows)) {
    temp = df_cat_iter %>% filter(ptotal == p, nrows == n) %>% mutate(nclasses = as.factor(nclasses)) %>% select(binary, ridge, nclasses, ptotal, nrows)
    mod = lm(binary ~  0 + ridge*nclasses, data = temp)
    params = coef(mod)

    temp_max = temp %>% group_by(nclasses) %>% filter(ridge == max(ridge))

    ll_empty = list()
    for (i in seq_len(nrow(temp_max))) {
      pred = predict(mod, temp_max[i,])
      if (temp_max[i,"nclasses",drop = TRUE] %in% mod$xlevels$nclasses) {
        ie = paste0("ridge:nclasses", temp_max[i,"nclasses",drop=TRUE])
        if (ie %in% names(params)) {
          ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = params["ridge"] + params[ie])
          #if (is.na(coef(summary(mod))[,"Pr(>|t|)"][ie])) {
            #ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = params["ridge"])
          #} else {
            #if (coef(summary(mod))[,"Pr(>|t|)"][ie] < 0.05) {
              #ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = params["ridge"] + params[ie])
            #} else {
              #ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = NA)
            #}
          #}
        } else {
          ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = params["ridge"])
        }
      }
    }
    preds = do.call(rbind, ll_empty)
    preds$labels = as.character(round(preds$label, 2))
    ll_temp[[k]] = preds
    k = k + 1
  }
}
df_labels = do.call(rbind, ll_temp)

gg_iter = df_cat_iter %>%
  ggplot(aes(x = ridge, y = binary, color = as.factor(nclasses))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(intercept = 0, slope = 1, color = "dark red", linetype = "dashed", alpha = 0.5) +
    ggrepel::geom_label_repel(data = df_labels, aes(x = ridge, y = binary, fill = factor(nclasses, levels = c("5", "10", "20")), label = labels),
      colour = "white", fontface = "bold", show.legend = FALSE) +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    #scale_color_brewer(palette = "Set1") +
    #scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_family = "Gyre Bonum") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 9 * font_scale)
    ) +
    xlab("Iterations (Ridge)") +
    ylab("Iterations (Binary)") +
    labs(color = "Number of classes\nper feature") +
    scale_x_continuous(breaks = c(0, 5000, 10000)) +
    facet_grid(nrows ~ ptotal)

dinA4width = 210 * font_scale
ggsave(plot = gg_iter, filename = "categorical_iters.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.6, units = "mm")







