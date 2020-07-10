base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/categorical/runtime")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

library(compboost)

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir)


n_classes = c(5, 10, 20)
p_inf_classes = c(0, 0.5)

config_classes = expand.grid(ncls = n_classes, pic = p_inf_classes)
config_classes$nic = trunc(config_classes$ncls * config_classes$pic)
config_classes$pic = NULL


for (ncls in config_classes$ncls) {
  for (nic in config_classes$nic) {

    nm_save = paste0("xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, "-nclasses", ncls, "-informative-classes", nic, ".Rda")

    seed = trunc(config$n / (config$p + config$pnoise + ncls + nic) * config$sn_ratio)

    set.seed(seed)
    dat = simCategoricalData(config$n, config$p, config$pnoise, nclasses = ncls, ncnoise = nic)
    dat_noise = dat$data

    set.seed(seed * config$rep)
    dat_noise$y = rnorm(n = config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / config$sn_ratio)

    cnames = colnames(dat_noise)
    mstop = 2000L



    ## Write compboost code here:
    ## ------------------------------------

    ## Linear base-learner

    time_start_linear = proc.time()

    cboost_linear = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
    temp = lapply(cnames[cnames != "y"], function (feat) {
      cboost_linear$addBaselearner(feat, "cat", BaselearnerPolynomial)
    })

    cboost_linear$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
      max_time = 0, time_unit = "seconds")

    time_init_linear = proc.time() - time_start_linear
    temp = capture.output({
      cboost_linear$train(mstop, trace = 0)
    })
    time_fit_linear = proc.time() - time_start_linear + time_init_linear

    ## Binary base-learner

    time_start_binary = proc.time()

    response_binary = ResponseRegr$new("y", as.matrix(dat_noise$y))

    factory_list_binary = BlearnerFactoryList$new()
    temp = lapply(cnames[-which(cnames == "y")], function (fn) {
      cdata_source = CategoricalData$new(dat_noise[[fn]], paste0("x", fn))
      bl_list = lapply(unique(dat_noise[[fn]]), function (cl) {
        bl = BaselearnerCategoricalBinary$new(cdata_source, cl)
        factory_list_binary$registerFactory(bl)
      })
      return (cdata_source)
    })

    optimizer = OptimizerCoordinateDescent$new()

    log_iterations = LoggerIteration$new(" iterations", TRUE, mstop)
    log_time = LoggerTime$new("time", FALSE, 0, "seconds")

    logger_list = LoggerList$new()
    logger_list$registerLogger(log_iterations)
    logger_list$registerLogger(log_time)

    loss_quadratic = LossQuadratic$new()
    cboost_binary = Compboost_internal$new(
      response      = response_binary,
      learning_rate = 0.05,
      stop_if_all_stopper_fulfilled = FALSE,
      factory_list = factory_list_binary,
      loss         = loss_quadratic,
      logger_list  = logger_list,
      optimizer    = optimizer
    )

    time_init_binary = proc.time() - time_start_binary
    temp = capture.output({
      cboost_binary$train(trace = 0)
    })
    time_fit_binary = proc.time() - time_start_binary + time_init_binary

    ## Ridge base-learner

    time_start_ridge = proc.time()

    response_ridge = ResponseRegr$new("y", as.matrix(dat_noise$y))

    factory_list_ridge = BlearnerFactoryList$new()
    temp = lapply(cnames[-which(cnames == "y")], function (fn) {
      cdata_source = CategoricalData$new(dat_noise[[fn]], paste0("x", fn))
      bl = BaselearnerCategoricalRidge$new(cdata_source, list(df = ncls))
      factory_list_ridge$registerFactory(bl)
    })

    optimizer = OptimizerCoordinateDescent$new()

    log_iterations = LoggerIteration$new(" iterations", TRUE, mstop)
    log_time = LoggerTime$new("time", FALSE, 0, "seconds")

    logger_list = LoggerList$new()
    logger_list$registerLogger(log_iterations)
    logger_list$registerLogger(log_time)

    loss_quadratic = LossQuadratic$new()
    cboost_ridge = Compboost_internal$new(
      response      = response_ridge,
      learning_rate = 0.05,
      stop_if_all_stopper_fulfilled = FALSE,
      factory_list = factory_list_ridge,
      loss         = loss_quadratic,
      logger_list  = logger_list,
      optimizer    = optimizer
    )

    time_init_ridge = proc.time() - time_start_ridge
    temp = capture.output({
      cboost_ridge$train(trace = 0)
    })
    time_fit_ridge = proc.time() - time_start_ridge + time_init_ridge

    ## ------------------------------------

    ## Binning

    time_start_binning = proc.time()

    cboost_binning = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
    temp = lapply(cnames[cnames != "y"], function (feat) {
                    cboost_binning$addBaselearner(feat, "spline", BaselearnerPSpline, bin_root = 2)
    })

    cboost_binning$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
                             max_time = 0, time_unit = "seconds")

    time_init_binning = proc.time() - time_start_binning
    temp = capture.output({
      cboost_binning$train(mstop, trace = 0L)
    })
    time_fit_binning = proc.time() - time_start_binning + time_init_binning

    ## ------------------------------------

    ## Save results:

    bm_extract = list(
      date      = as.character(Sys.time()),
      data_seed = seed,
      config    = config,
      config_classes = config_classes,

      log_linear     = cboost_linear$getLoggerData(),
      time_linear    = c(init = time_init_linear[3], fit = time_fit_linear[3]),

      log_ridge      = cboost_ridge$getLoggerData(),
      time_ridge     = c(init = time_init_ridge[3], fit = time_fit_ridge[3]),

      log_binary     = cboost_binary$getLoggerData(),
      time_binary    = c(init = time_init_binary[3], fit = time_fit_binary[3])
    )

    save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))
  }
}




