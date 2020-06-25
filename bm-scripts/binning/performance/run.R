base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/binning/performance")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir)

nm_save = paste0("xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, ".Rda")

## Simulate data and create data with noise:
seed = trunc(config$n / (config$p + config$pnoise) * config$sn_ratio)

set.seed(seed)
dat = simData(config$n, config$p, config$pnoise)
dat_noise = dat$data

set.seed(seed * config$rep)
dat_noise$y = rnorm(n = config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / config$sn_ratio)

cnames = colnames(dat_noise)
max_mstop = 20000L

eps_for_break = 0.001
patience = 5L

library(compboost)


## Write compboost code here:
## ------------------------------------

## No binning

time_start_nobinning = proc.time()

cboost_nobinning = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_nobinning$addBaselearner(feat, "spline", BaselearnerPSpline, df = 5)
})

cboost_nobinning$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

oob_response = cboost_nobinning$prepareResponse(dat$data$y)
oob_data = cboost_nobinning$prepareData(dat$data)
cboost_nobinning$addLogger(logger = LoggerOobRisk, use_as_stopper = TRUE, logger_id = "oob_risk",
  used_loss = LossQuadratic$new(), eps_for_break = eps_for_break, patience = patience, oob_data = oob_data,
  oob_response = oob_response)

time_init_nobinning = proc.time() - time_start_nobinning
temp = capture.output({
  cboost_nobinning$train(max_mstop, trace = 0L)
})
time_fit_nobinning = proc.time() - time_start_nobinning + time_init_nobinning



## ------------------------------------

## Binning

time_start_binning = proc.time()

cboost_binning = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_binning$addBaselearner(feat, "spline", BaselearnerPSpline, bin_root = 2, df = 5)
})

cboost_binning$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

oob_response = cboost_binning$prepareResponse(dat$data$y)
oob_data = cboost_binning$prepareData(dat$data)
cboost_binning$addLogger(logger = LoggerOobRisk, use_as_stopper = TRUE, logger_id = "oob",
  used_loss = LossQuadratic$new(), eps_for_break = eps_for_break, patience = patience, oob_data = oob_data,
  oob_response = oob_response)


cboost_binning$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

time_init_binning = proc.time() - time_start_binning
temp = capture.output({
  cboost_binning$train(max_mstop, trace = 0L)
})
time_fit_binning = proc.time() - time_start_binning + time_init_binning


## ------------------------------------

## Save results:

bm_extract = list(
  date      = as.character(Sys.time()),
  data_seed = seed,
  config    = config,
  log_nobinning  = cboost_nobinning$getLoggerData(),
  time_nobinning = c(init = time_init_nobinning[3], fit = time_fit_nobinning[3]),
  coef_nobinning = cboost_nobinning$getEstimatedCoef(),
  trace_nobinning = cboost_nobinning$getSelectedBaselearner(),
  log_binning    = cboost_binning$getLoggerData(),
  time_binning   = c(init = time_init_binning[3], fit = time_fit_binning[3]),
  coef_binning   = cboost_binning$getEstimatedCoef(),
  trace_binning = cboost_binning$getSelectedBaselearner()
)

save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))




#feat = 10

#gg = cboost_nobinning$plot(paste0("x", feat, "_spline"))

#df_plot = data.frame(x = dat$sim_poly[[feat]]$x, y = dat$sim_poly[[feat]]$y)
#gg + ggplot2::geom_point(data = df_plot, ggplot2::aes(x = x, y = y), alpha = 0.5)
