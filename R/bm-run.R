#' Run benchmark
#'
#' @param configs [data.frame()] Configuration for the data
#' @param bm_dirs [character()] Vector of direcotries containing the code `run.R` for benchmarking
#' @return Nothing, the results are written into single files
runBM = function (configs, bm_dirs)
{
  chk = checkBMData(configs, FALSE)

  nconfigs = nrow(configs)
  for (i in seq_len(nconfigs)) {
    for (bmd in bm_dirs) {
      msg_trace = paste0(i, "/", nconfigs, ": dim(data)=", configs[i,"n"], "x(",
        configs[i,"p"], "+", configs[i,"pnoise"] ,"),  rep=", configs[i, "rep"],
        ",  signal-to-noise-ratio=", configs[i,"sn_ratio"], ",  bm-dir=", bmd,
        "\n")

      cat(msg_trace)

      saveConfig(configs[i,], bmd)
      system(paste0("Rscript ", bmd, "/run.R"))
    }
  }
}


#' Write configuration on disk
#'
#' @param config [data.frame()] Data frame with one row containing the configuration
#' @param path [character(1L)] Directory where the file should be written to
saveConfig = function (config, path = getwd())
{
  chk = checkBMData(config, FALSE)
  if (nrow(config) != 1) stop("Just one configuration should be saved!")

  save(config, file = paste0(path, "/config.Rda"))
}

#' Load configuration from disk
#'
#' @param path [character(1L)] Directory with the `config.Rda` file in it
loadConfig = function (path = getwd())
{
  load(paste0(path, "/config.Rda"))
  chk = checkBMData(config, FALSE)
  if (nrow(config) != 1) stop("Just one configuration should be loaded!")

  return(config)
}
