library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(viridis)

base_dir = here::here()

#source(paste0(base_dir, "/R/bm-sim-data.R"))
#source(paste0(base_dir, "/R/helper.R"))

sysfonts::font_add("Gyre Bonum",
    regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
    bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")
showtext::showtext_auto()

font_scale = 6


## Figure 1:
## =====================================

# Data:
# - `df_plt` data.frame
load(paste0(base_dir, "/paper-figures/rda/fig-1-agbm-restart-iters.Rda"))
agbm_iters = which.min(df_plt[df_plt$method == "ACWB", "Risk"])
iter = sum(df_plt$method == "ACWB")

# Plot
gg = ggplot(
    df_plt %>% filter(type == "oob"),
    aes(x = Iteration, y = Risk, color = method)) +
  geom_vline(
    xintercept = agbm_iters,
    color = "dark red",
    alpha = 0.5,
    linetype = "dashed",
    size = 1.3) +
  geom_line(size = 1.6) +
  xlim(0, iter) +
  theme_minimal(base_family = "Gyre Bonum") +
  theme(
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale)
  ) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Iterations") +
  ylab("Empirical risk on\ntest data") +
  labs(color = "Algorithm") +
  annotate("text",
    x = agbm_iters,
    y = max(df_plt$Risk),
    label = "Optimal stopping ACWB",
    color = "dark red",
    hjust = -0.1,
    size = 8)

dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "figures/fig-1-optim_emp_risk.pdf",
  width = dinA4width * 2/3 * 0.5,
  height = dinA4width * 2/3 * 0.7 * 0.5,
  units = "mm")




## Figure 2:
## =====================================

# Data:
# - `ll_feats` list with
#   $ .. `categorical` data.frame
#   $ .. `numeric` data.frame
load(paste0(base_dir, "/paper-figures/rda/fig-2-features-viz.Rda"))

## NUMERIC

gg = ggplot(data = ll_feats$numeric, aes(x = x, y = y)) +
  geom_line(size = 1.2) +
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
  ylab(expression(eta[j])) +
  facet_wrap(. ~ feat, scales = "free", ncol = 3)

dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "figures/fig-2-fe-numeric.pdf",
  width = dinA4width * 2/3 * 0.5,
  height = dinA4width * 2/3 * 0.5,
  units = "mm")

## CATEGORICAL

gg = ggplot(data = ll_feats$categorical, aes(x = param.cls, y = param.means)) +
  geom_boxplot() +
  theme_minimal(base_family = "Gyre Bonum") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    #axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text.x = element_blank(),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale)
  ) +
  ylab(expression(paste("Group means ", tau[j]))) +
  xlab("Group") +
  facet_wrap(. ~ feat, scales = "free", ncol = 3)

dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "figures/fig-2-fe-cat.pdf",
  width = dinA4width * 2/3 * 0.5,
  height = dinA4width * 2/3 * 0.5,
  units = "mm")



## Figure 3:
## =====================================

# Data:
# - `df_plt_mem` data.frame
load(paste0(base_dir, "/paper-figures/rda/fig-3-binning-memory.Rda"))
# - `df_plt_run` data.frame
load(paste0(base_dir, "/paper-figures/rda/fig-3-binning-runtime.Rda"))

## MEMORY
gg = ggplot(
    data = df_plt_mem,
    aes(x = nrows, y = rel_mem, color = ptotal, group = paste0(ncols, ncolsnoise))) +
  geom_hline(
    yintercept = 1,
    color = "dark red",
    lty = 2) +
  #geom_line() +
  geom_smooth(se = FALSE, alpha = 0.7) +
  geom_point(size = 10, alpha = 0.7) +
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
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  scale_y_continuous(breaks = c(1, 2, 4, 6)) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of Rows\n(log10 Scale)") +
  ylab(expression(atop("Memory improvement\n", paste(Mem["No Binning"], "/", Mem["Binning"], sep = "")))) +
  labs(color = "Number of\nFeatures") +
  annotate("text",
    x = max(df_plt_mem$nrows),
    y = 1,
    label = "Baseline (used memory is equal)",
    color = "dark red",
    vjust = 1.5,
    hjust = 1,
    size = 1.5 * font_scale)

dinA4width = 210 * font_scale
scale_fac = 1/2
ggsave(
  plot = gg,
  filename = "figures/fig-3-binning-memory.pdf",
  width = dinA4width * scale_fac,
  height = dinA4width * scale_fac * 0.7,
  units = "mm")


## RUNTIME:

df_plt_run = df_plt_run %>%
  filter(rel_time < 10, rel_time > 2, phase == "Fitting") %>%
  group_by(nrows, ptotal) %>%
  summarize(med = median(rel_time), min = min(rel_time), max = max(rel_time))

dodge_width = 0.25

gg = ggplot() +
  geom_hline(
    yintercept = 1,
    lty = 2,
    col = "dark red") +
  geom_point(
    data = df_plt_run,
    aes(x = nrows, y = med, color = as.factor(ptotal)),
    size = 10,
    alpha = 0.7,
    position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    data = df_plt_run,
    aes(x = nrows, ymax = max, ymin = min, color = as.factor(ptotal)),
    na.rm = TRUE,
    position = position_dodge(width = dodge_width),
    size = 1.3) +
  #geom_violin(
    #data = df_plt_run ,
    #aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal)),
    #alpha = 0.2,
    #show.legend = FALSE) +
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
  xlab("Number of Rows\n(log10 Scale)") +
  ylab(expression(atop("Speedup\n", paste(Time["No Binning"], "/", Time["Binning"], sep = "")))) +
  labs(color = "Number of\nFeatures", fill = "Number of\nFeatures") +
  scale_y_continuous(breaks = c(1, 2, 4, 6)) +
  #scale_x_continuous(trans = "log10")# +
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  annotate("text",
    x = max(df_plt_mem$nrows),
    y = 1,
    label = "Baseline (runtime is equal)",
    color = "dark red",
    vjust = 1.5,
    hjust = 1,
    size = 1.5 * font_scale)
  #facet_grid(. ~ factor(phase, levels = c("Initialization", "Fitting", "Initialization + Fitting")), scales = "free_y")

dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "figures/fig-3-binning-runtime.pdf",
  width = dinA4width * scale_fac,
  height = dinA4width * 0.7 * scale_fac,
  units = "mm")





## Figure 3:
## =====================================


seed = 31415

n = 10000
p = 4
pnoise = 2
sn_ratio = 0.4

set.seed(seed)
dat = simData(n, p, pnoise)
dat_noise = dat$data
set.seed(seed)
dat_noise$y = rnorm(n = n, mean = dat_noise$y, sd = sd(dat_noise$y) / sn_ratio)

library(compboost)

set.seed(seed)
cboost = boostSplines(data = dat_noise, target = "y", iterations = 10000L, learning_rate = 0.01,
  loss = LossQuadratic$new(), stop_args = list(eps_for_break = 0, patience = 3L), oob_fraction = 0.3,
  df = 7)

set.seed(seed)
cboost_bin = boostSplines(data = dat_noise, target = "y", iterations = 10000L, learning_rate = 0.01,
  loss = LossQuadratic$new(), stop_args = list(eps_for_break = 0, patience = 3L), oob_fraction = 0.3,
  bin_root = 2, df = 7)



ndata = 1000L
dat_idx = as.integer(seq(1, n, len = ndata))

feat = colnames(dat$data)[grepl(pattern = "x", x = colnames(dat$data))]
bls = paste0(feat, "_spline")
coef_names = c("coef_binning", "coef_nobinning")
coefs = list(coef_binning = cboost_bin$getEstimatedCoef(), coef_nobinning = cboost$getEstimatedCoef())

out = list()
for(bl in bls) {
  bl_nbr = as.numeric(gsub("\\D", "", bl))

  x = dat$data[[paste0("x", bl_nbr)]][dat_idx]
  y = dat$sim_poly[[bl_nbr]]$y[dat_idx]

  df_temp = data.frame(x = x, truth = y)

  knots = compboostSplines::createKnots(values = x, n_knots = 20, degree = 3)
  basis = compboostSplines::createSplineBasis(values = x, degree = 3, knots = knots)

  for (cn in coef_names) {
    params = coefs[[cn]]
    if (bl %in% names(params)) {
      param = params[[bl]]
      pred = basis %*% param
      df_pred = data.frame(pred)
    } else {
      df_pred = data.frame(rep(0, ndata))
    }
    colnames(df_pred) = cn
    df_temp = cbind(df_temp, df_pred, bl = bl)
  }
  out[[bl]] = df_temp
}

ll_fe = lapply(out, function (df) {
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

df_fe$line = factor(df_fe$line)
df_fe$line = ordered(df_fe$line, c("Truth", "No Binning", "Binning"))
df_fe$linetype = ifelse(df_fe$line == "Binning", "solid", "dashed")

df_area = cbind(
  df_fe %>% select(x, bl, method, y, feat, line) %>% filter(line == "No Binning"),
  df_fe %>% ungroup() %>% mutate(y_t = y, line_t = line) %>% select(y_t, line_t) %>% filter(line_t == "Truth"))

gg = ggplot() +
  geom_ribbon(
    data = df_area,
    aes(ymin = y, ymax = y_t, x = x),
    fill = "dark red",
    alpha = 0.2) +
  geom_line(
    data = df_fe,
    aes(x = x, y = y, color = line, linetype = linetype),
    lwd = 2) +
  #scale_color_viridis(discrete = TRUE) +
  theme_minimal(base_family = "Gyre Bonum") +
  scale_color_viridis(discrete = TRUE) +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale),
    panel.grid.major.x = element_blank()) +
  xlab("x") +
  ylab("Partial effect") +
  labs(color = "", linetype = "") +
  #scale_x_continuous(breaks = NULL) +
  scale_linetype(guide = "none") +
  facet_wrap(. ~ feat, scales = "free_x")#, scales = "free")
gg


dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "figures/fig-4-binning-fe.pdf",
  width = dinA4width * 1/2,
  height = dinA4width * 0.7 * 1/2,
  units = "mm")



## Figure 5:
## =====================================

# Data:
# - `df_imse` data.frame
load(paste0(base_dir, "/paper-figures/rda/fig-5-binning-imse.Rda"))

df_imse$method_n = as.factor(df_imse$method_n)
levels(df_imse$method_n) = c("Binning", "Binning 4", "No binning")

gg = ggplot(
    data = df_imse %>% filter(method_n != "Binning 4"),
    aes(x = as.factor(nrows), y = mimse, fill = method_n, color = method_n)) +
  geom_boxplot(alpha = 0.2) +
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
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Number of rows\n(log10 scale)") +
  ylab("MISE") +
  labs(color = "", fill = "") +
  facet_grid(paste0("SNR = ", sn_ratio) ~ ., scales = "free_y")
gg

dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "figures/fig-5-binning-imse.pdf",
  width = dinA4width * 1/2,
  height = dinA4width * 1/2 * 0.7,
  units = "mm")




## Figure 6:
## =====================================

# Data:
# - `df_plt_mem` data.frame
load(paste0(base_dir, "/paper-figures/rda/fig-6-cat-memory.Rda"))
# - `df_plt_run` data.frame
load(paste0(base_dir, "/paper-figures/rda/fig-6-cat-runtime.Rda"))

font_scale = 6

## MEMORY

df_plt_mem = df_cat_memory %>%
  filter(method != "linear") %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  summarize(mem = median(mem)) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  mutate(method = as.factor(method)) %>%
  filter(ptotal %in% c(10, 40, 75, 100, 150, 300))

levels(df_plt_mem$method) = list(Binary = "binary", Ridge = "ridge")
df_plt_mem$ptotal = ordered(df_plt_mem$ptotal, levels = sort(unique(df_plt_mem$ptotal)))
df_plt_mem$nclasses = factor(paste0("# Classes: ", df_plt_mem$nclasses), levels = paste0("# Classes: ", c(5, 10, 20)))

gg = ggplot() +
  geom_smooth(
    data = df_plt_mem,
    aes(x = nrows, y = mem, color = ptotal, group = paste0(ncols, ncolsnoise, nclasses)),
    se = FALSE) +
  geom_point(
    data = df_plt_mem,
    aes(x = nrows, y = mem, color = ptotal, group = paste0(ncols, ncolsnoise)),
    size = 6,
    alpha = 0.5) +
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
  scale_x_continuous(breaks = sort(unique(df_plt_mem$nrows)), trans = "log10") +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of rows\n(log10 Scale)") +
  ylab("Used memory in MB") +
  labs(color = "Number of\nreatures") +
  coord_cartesian(clip = 'off') +
  facet_grid(nclasses ~ method)#, scales= "free_y")
gg

dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "../../paper-figures/figures/fig-6-cat-memory.pdf",
  #filename = "figures/fig-6-cat-memory.pdf",
  width = dinA4width * 1/2,
  height = dinA4width * 1/2,
  units = "mm")

## RUNTIME

df_plt_run = df_cat_runtime %>%
  filter(method != "linear") %>%
  #group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  #summarize(mem = median(time)) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  mutate(method = as.factor(method)) %>%
  group_by(nrows, ptotal, method, nclasses) %>%
  summarize(med = median(time), min = min(time), max = max(time)) %>%
  filter(ptotal %in% c(10, 40, 75, 100, 150, 300))

levels(df_plt_run$method) = list(Binary = "binary", Ridge = "ridge")
df_plt_run$ptotal = ordered(df_plt_run$ptotal, levels = sort(unique(df_plt_run$ptotal)))

df_plt_run$nclasses = factor(paste0("# Classes: ", df_plt_run$nclasses), levels = paste0("# Classes: ", c(5, 10, 20)))

dodge_width = 0.25

gg = ggplot() +
  geom_point(
    data = df_plt_run,
    aes(x = nrows, y = med / 60, color = as.factor(ptotal)),
    size = 6,
    alpha = 0.5,
    position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    data = df_plt_run,
    aes(x = nrows, ymax = max / 60, ymin = min / 60, color = as.factor(ptotal)),
    na.rm = TRUE,
    position = position_dodge(width = dodge_width),
    size = 1.3) +
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
  xlab("Number of rows\n(log10 Scale)") +
  ylab("Runtime in minutes") +
  labs(color = "Number of\nfeatures", fill = "Number of\nfeatures") +
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  facet_grid(nclasses ~ method)#, scales= "free_y")


dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  #filename = "figures/fig-6-cat-runtime.pdf",
  filename = "../../paper-figures/figures/fig-6-cat-runtime.pdf",
  width = dinA4width * 1/2,
  height = dinA4width * 1/2,
  units = "mm")



## Figure 7:
## =====================================

# Data:
# - `ll_noise` list with
#   $ .. `density` data.frame
#   $ .. `cat_sel` data.frame
load(paste0(base_dir, "/paper-figures/rda/fig-7-cat-noise.Rda"))

gg = ggplot(
    mapping = aes(
      x = rel_nwrongnotselected,
      y = rel_notselected,
      shape = method,
      color = method,
      fill = method)) +
  geom_polygon(
    data = ll_noise$density,
    alpha = 0.2,
    size = 0.1) +
  geom_point(data = ll_noise$cat_sel) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  xlab("FNR") +
  ylab("TNR") +
  labs(fill = "", color = "", shape = "") +
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
  xlim(min(ll_noise$density$rel_nwrongnotselected), max(ll_noise$denstiy$rel_nwrongnotselected)) +
  ylim(min(ll_noise$density$rel_notselected), max(ll_noise$density$rel_notselected)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  facet_grid(sn_ratiof ~ .)#, scales = "free_y")
gg


dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "figures/fig-7-cat-noise.pdf",
  width = dinA4width * 2/3 * 0.7,
  height = dinA4width * 2/3 * 0.5,
  units = "mm")



## Figure 8:
## =====================================

# Data:
# - `df_cat_bp` data.frame
load(paste0(base_dir, "/paper-figures/rda/fig-8-cat-mse.Rda"))

font_scale = 6

gg = ggplot(df_cat_bp, aes(x = mse, y = value, fill = method, color = method)) +
  geom_boxplot(alpha = 0.2) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  xlab("") +
  ylab("MSE") +
  labs(fill = "", color = "", shape = "") +
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
  ylim(0, 40) +
  facet_grid(sn_ratiof ~ .) #, scales = "free_y")

dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "figures/fig-8-cat-mse.pdf",
  width = dinA4width * 2/3 * 0.7,
  height = dinA4width * 2/3 * 0.5,
  units = "mm")


## Figure 9:
## =====================================

## MEMORY:

load("rda/fig-9-acwb-mem.Rda")

gg = ggplot(
    data = df_plt_mem %>% filter(ptotal %in% c(10, 30, 75, 100, 150, 300)),
    aes(x = nrows, y = rel_mem, color = as.factor(ptotal), group = paste0(ncols, ncolsnoise))) +
  geom_hline(
    yintercept = 1,
    color = "dark red",
    lty = 2) +
  #geom_line() +
  geom_smooth(se = FALSE, alpha = 0.7) +
  geom_point(size = 10, alpha = 0.7) +
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
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of Rows\n(log10 Scale)") +
  ylab(expression(atop("Memory improvement\n", paste(Mem["CWB"], "/", Mem["ACWB"], sep = "")))) +
  labs(color = "Number of\nFeatures") +
  annotate("text",
    x = max(df_plt_mem$nrows),
    y = 1,
    label = "Baseline (used memory is equal)",
    color = "dark red",
    vjust = 1.5,
    hjust = 1,
    size = 1.5 * font_scale)

dinA4width = 210 * font_scale
scale_fac = 1/2
ggsave(
  plot = gg,
  filename = "figures/fig-9-acwb-memory.pdf",
  width = dinA4width * scale_fac,
  height = dinA4width * scale_fac * 0.7,
  units = "mm")


## RUNTIME:

load("rda/fig-9-acwb-run.Rda")

df_plt_run = df_plt_run %>%
  filter(rel_time < 0.7) %>%
  group_by(nrows, ptotal) %>%
  summarize(med = median(rel_time), min = min(rel_time), max = max(rel_time)) %>%
  filter(ptotal %in% c(10, 30, 75, 100, 150, 300))

dodge_width = 0.25

gg = ggplot() +
  geom_hline(
    yintercept = 1,
    lty = 2,
    col = "dark red") +
  geom_point(
    data = df_plt_run,
    aes(x = nrows, y = med, color = as.factor(ptotal)),
    size = 10,
    alpha = 0.7,
    position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    data = df_plt_run,
    aes(x = nrows, ymax = max, ymin = min, color = as.factor(ptotal)),
    na.rm = TRUE,
    position = position_dodge(width = dodge_width),
    size = 1.3) +
  #geom_violin(
    #data = df_plt_run ,
    #aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal)),
    #alpha = 0.2,
    #show.legend = FALSE) +
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
  xlab("Number of Rows\n(log10 Scale)") +
  ylab(expression(atop("Speedup\n", paste(Time["No Binning"], "/", Time["Binning"], sep = "")))) +
  labs(color = "Number of\nFeatures", fill = "Number of\nFeatures") +
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  annotate("text",
    x = max(df_plt_mem$nrows),
    y = 1,
    label = "Baseline (runtime is equal)",
    vjust = 1.5,
    color = "dark red",
    hjust = 1,
    size = 1.5 * font_scale) +
  ylim(0.25, 1)

dinA4width = 210 * font_scale
ggsave(
  plot = gg,
  filename = "figures/fig-9-acwb-runtime.pdf",
  width = dinA4width * scale_fac,
  height = dinA4width * 0.7 * scale_fac,
  units = "mm")






