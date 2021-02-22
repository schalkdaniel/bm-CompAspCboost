cargs = commandArgs(trailingOnly=TRUE)
base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/optimizer/agbm-mom")
config_file = paste0(base_sub_dir, "/config", cargs, ".Rmd")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir, cargs)
mom = config$mom

tr = 0L

### For testing:
test = FALSE
if (test) {
  mom = 0.00005
  config = list(n = 10000, p = 20, pnoise = 10, sn_ratio = 10, rep = 1)
  tr = 100L
}

nm_save = paste0("res-results/restart-xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, "-", digest::sha1(mom), ".Rda")

## Simulate data and create data with noise:
seed = trunc(config$n / (config$p + config$pnoise) * config$sn_ratio)

set.seed(seed)
dat = simData(config$n, config$p, config$pnoise)
dat_noise = dat$data

set.seed(seed * config$rep)
dat_noise$y = rnorm(n = config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / config$sn_ratio)

cnames = colnames(dat_noise)
max_mstop = 50000L

eps_for_break = 0
patience = 5L

library(compboost)


## Write compboost code here:
## ------------------------------------

## Coordinate Descent

time_start_cod = proc.time()

cboost_cod = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_cod$addBaselearner(feat, "spline", BaselearnerPSpline, df = 5)
})

cboost_cod$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

oob_response = cboost_cod$prepareResponse(dat$data$y)
oob_data = cboost_cod$prepareData(dat$data)
cboost_cod$addLogger(logger = LoggerOobRisk, use_as_stopper = TRUE, logger_id = "oob_risk",
  used_loss = LossQuadratic$new(), eps_for_break = eps_for_break, patience = patience, oob_data = oob_data,
  oob_response = oob_response)

time_init_cod = proc.time() - time_start_cod
temp = capture.output({
  cboost_cod$train(max_mstop, trace = tr)
})
time_fit_cod = proc.time() - time_start_cod + time_init_cod



## ------------------------------------

## AGBM

cboost_agbm = Compboost$new(dat_noise, "y", loss = LossQuadratic$new(), optimizer = OptimizerAGBM$new(mom))
time_start_agbm = proc.time()

temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_agbm$addBaselearner(feat, "spline", BaselearnerPSpline, df = 5)
})

cboost_agbm$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

oob_response = cboost_agbm$prepareResponse(dat$data$y)
oob_data = cboost_agbm$prepareData(dat$data)
cboost_agbm$addLogger(logger = LoggerOobRisk, use_as_stopper = TRUE, logger_id = "oob",
  used_loss = LossQuadratic$new(), eps_for_break = eps_for_break, patience = patience, oob_data = oob_data,
  oob_response = oob_response)

cboost_agbm$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

time_init_agbm = proc.time() - time_start_agbm
temp = capture.output({
  cboost_agbm$train(max_mstop, trace = tr)
})
time_fit_agbm = proc.time() - time_start_agbm + time_init_agbm


iter_agbm = length(cboost_agbm$getInbagRisk() - 1)


#### Restart:
time_start_restart = proc.time()

cboost_restart = Compboost$new(dat_noise, "y", loss = LossQuadratic$new(cboost_agbm$predict(), TRUE), optimizer = OptimizerCoordinateDescent$new())

temp_restart = lapply(cnames[cnames != "y"], function (feat) {
                        cboost_restart$addBaselearner(feat, "spline", BaselearnerPSpline, df = 5)
})

cboost_restart$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
                         max_time = 0, time_unit = "seconds")

oob_response_restart = cboost_restart$prepareResponse(dat$data$y)
oob_data_restart = cboost_restart$prepareData(dat$data)
cboost_restart$addLogger(logger = LoggerOobRisk, use_as_stopper = TRUE, logger_id = "oob",
                         used_loss = LossQuadratic$new(oob_response$getPrediction(), TRUE), eps_for_break = eps_for_break, patience = patience, oob_data = oob_data_restart,
                         oob_response = oob_response_restart)

cboost_restart$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
                         max_time = 0, time_unit = "seconds")

time_init_restart = proc.time() - time_start_restart
temp = capture.output({
  cboost_restart$train(max_mstop - iter_agbm, trace = tr)
})
time_fit_restart = proc.time() - time_start_restart + time_init_restart

time_fit_agbm = proc.time() - time_start_agbm + time_init_agbm


## Save results:
## ------------------------------------


if (test) {
  risk = cboost_cod$getInbagRisk()
  risk_oob = cboost_cod$getLoggerData()$oob
  df_plot_cod = data.frame(risk = c(risk, risk_oob), iter = c(seq_along(risk), seq_along(risk_oob)),
    type = rep(c("inbag", "oob"), c(length(risk), length(risk_oob))), method = "COD")

  inbag_risk_agbm = cboost_agbm$getInbagRisk()
  inbag_risk_restart = cboost_restart$getInbagRisk()
  risk = c(inbag_risk_agbm, inbag_risk_restart)
  risk_oob = c(cboost_agbm$getLoggerData()$oob, cboost_restart$getLoggerData()$oob)
  df_plot_agbm = data.frame(risk = c(risk, risk_oob), iter = c(seq_along(risk), seq_along(risk_oob)),
    type = rep(c("inbag", "oob"), c(length(risk), length(risk_oob))), method = "AGBM")

  library(ggplot2)
  ggplot(rbind(df_plot_cod, df_plot_agbm), aes(x = iter, y = risk, color = type, linetype = method)) + geom_line()
}

bm_extract = list(
  date = as.character(Sys.time()),
  momentum  = mom,
  seed      = seed,
  cols = config$p,
  rows = config$n,
  pnoise = config$pnoise,
  snr = config$sn_ratio,
  rep = config$rep,

  iters_cod = length(cboost_cod$getSelectedBaselearner()),
  iters_agbm = length(cboost_agbm$getSelectedBaselearner()),
  iters_restart = length(cboost_restart$getSelectedBaselearner()),

  time_cod_init = time_init_cod[3],
  time_cod_fit  = time_fit_cod[3],
  time_agbm_init = time_init_agbm[3],
  time_agbm_fit  = time_fit_agbm[3],
  time_restart_init = time_init_restart[3],
  time_restart_fit  = time_fit_restart[3],

  log_cod  = list(cboost_cod$getLoggerData()),
  log_agbm    = list(cboost_agbm$getLoggerData()),
  log_restart = list(cboost_restart$getLoggerData()),

  trace_cod = list(cboost_cod$getSelectedBaselearner()),
  trace_agbm = list(cboost_agbm$getSelectedBaselearner()),
  trace_agbm_mom = list(cboost_agbm$optimizer$getSelectedMomentumBaselearner()),
  trace_restart = list(cboost_restart$getSelectedBaselearner()),

  risk_cod = list(cboost_cod$getInbagRisk()),
  risk_agbm   = list(cboost_agbm$getInbagRisk()),
  risk_restart = list(cboost_restart$getInbagRisk()),

  coef_cod     = cboost_cod$getEstimatedCoef(),
  coef_agbm    = cboost_agbm$getEstimatedCoef(),
  coef_restart = cboost_restart$getEstimatedCoef()
)


#cbind(cboost_cod$predict(cboost_cod$data), cboost_cod$response$getResponse())
#cbind(cboost_agbm$predict(cboost_cod$data_oob) + cboost_restart$predict(cboost_cod$data_oob), cboost_cod$response_oob$getResponse())

my = cboost_cod$response$getResponse()
bm_extract$rsq_cod = sum((cboost_cod$predict(cboost_cod$data) - mean(my))^2) / sum((my - mean(my))^2)
bm_extract$rsq_agbm = sum((cboost_agbm$predict(cboost_cod$data) - mean(my))^2) / sum((my - mean(my))^2)
bm_extract$rsq_restart = sum((cboost_agbm$predict(cboost_cod$data) + cboost_restart$predict(cboost_cod$data) - mean(my))^2) / sum((my - mean(my))^2)

my = cboost_cod$response_oob$getResponse()
bm_extract$rsq_cod_oob = sum((cboost_cod$predict(cboost_cod$data_oob) - mean(my))^2) / sum((my - mean(my))^2)
bm_extract$rsq_agbm_oob = sum((cboost_agbm$predict(cboost_cod$data_oob) - mean(my))^2) / sum((my - mean(my))^2)
bm_extract$rsq_restart_oob = sum((cboost_restart$predict(cboost_cod$data_oob) - mean(my))^2) / sum((my - mean(my))^2)

bm_extract$mse_cod = min(bm_extract$risk_cod)
bm_extract$mse_agbm = min(bm_extract$risk_agbm)
bm_extract$mse_restart = min(bm_extract$risk_restart)

bm_extract$mse_cod_oob = min(bm_extract$log_cod[[1]]$oob)
bm_extract$mse_agbm_oob = min(bm_extract$log_agbm[[1]]$oob)
bm_extract$mse_restart_oob = min(bm_extract$log_restart[[1]]$oob)

library(dplyr)
library(tidyr)

getAllFeatEffectData = function (dat, bm_extract, ndata = 1000L, coef_names = character(0L))
{
  checkmate::assertCharacter(x = coef_names, min.len = 1L)
  if (any(! coef_names %in% names(bm_extract))) stop("coef_names needs to be in bm_extract")

  n = nrow(dat$data)
  checkmate::assertIntegerish(x = ndata, upper = bm_extract$rows, len = 1L, null.ok = TRUE)

  if (is.null(ndata)) ndata = n
  dat_idx = as.integer(seq(1, n, len = ndata))

  feat = colnames(dat$data)[grepl(pattern = "x", x = colnames(dat$data))]
  bls = paste0(feat, "_spline")

  out = list()
  for(bl in bls) {
    bl_nbr = as.numeric(gsub("\\D", "", bl))

    x = dat$data[[paste0("x", bl_nbr)]][dat_idx]
    y = dat$sim_poly[[bl_nbr]]$y[dat_idx]

    df_temp = data.frame(x = x, truth = y)

    knots = compboostSplines::createKnots(values = x, n_knots = 20, degree = 3)
    basis = compboostSplines::createSplineBasis(values = x, degree = 3, knots = knots)

    for (cn in coef_names) {
      params = bm_extract[[cn]]
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
  return (out)
}
getFeatureIME = function (x, truth, pred, loss = function (x,y) (x-y)^2)
{
  e = try({
    f_b = approxfun(x = x, y = loss(truth,pred))
    int = integrate(f = f_b, upper = max(x), lower = min(x))$value
  }, silent = TRUE)
  if (class(e) == "try-error") return (NA) else return (e)
  return ()
}



coef_names = paste0("coef_", c("cod", "agbm", "restart"))
feat_effects = getAllFeatEffectData(dat, bm_extract, coef_names = coef_names)

ll_imse = lapply(feat_effects, function (df) {
  df = df[,-which(names(df) == "bl")[-1]]
  df %>%
    mutate(coef_restart = coef_agbm + coef_restart) %>%
    pivot_longer(cols = coef_names, names_to = "method", values_to = "effect") %>%
    group_by(method) %>%
    mutate(y = effect - mean(effect), truth = truth - mean(truth)) %>%
    arrange(method, x)
})
df_imse = do.call(rbind, ll_imse)

df_imse_agg = df_imse %>%
  group_by(bl, method) %>%
  summarize(
    imse = getFeatureIME(x = x, truth = truth, pred = y),
    imae = getFeatureIME(x = x, truth = truth, pred = y, loss = function (f,y) abs(f - y))
  ) %>%
  group_by(method) %>%
  summarize(
    mise = mean(imse, na.rm = TRUE),
    miae = mean(imae, na.rm = TRUE)
  )

bm_extract$mise_cod = as.numeric(df_imse_agg[df_imse_agg$method == "coef_cod","mise"])
bm_extract$mise_agbm = as.numeric(df_imse_agg[df_imse_agg$method == "coef_agbm","mise"])
bm_extract$mise_restart = as.numeric(df_imse_agg[df_imse_agg$method == "coef_restart","mise"])

bm_extract$miae_cod = as.numeric(df_imse_agg[df_imse_agg$method == "coef_cod","miae"])
bm_extract$miae_agbm = as.numeric(df_imse_agg[df_imse_agg$method == "coef_agbm","miae"])
bm_extract$miae_restart = as.numeric(df_imse_agg[df_imse_agg$method == "coef_restart","miae"])

save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))

