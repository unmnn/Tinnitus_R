source(file.path(here::here(), "scirep_charite", "setup.R"))
source(file.path(here::here(), "scirep_charite", "helpers.R"))
source(file.path(here::here(), "scirep_charite", "train.R"))

library("tidyverse")
library("mlr")
library("parallelMap")
library("iml")
library("rlang")
library("patchwork")

.time_start <- Sys.time()

source(file.path(here::here(), "scirep_charite", "01_prep_data.R"))
source(file.path(here::here(), "scirep_charite", "02_define_grid.R"))

tg_file <- file.path("scirep_charite", "output", "grid", str_c(.config$pre, "_tg.rds"))
if(!dir.exists(dirname(tg_file))) dir.create(dirname(tg_file), recursive = TRUE, showWarnings = FALSE)

# if(!file.exists(tg_file)) {
  parallelStartSocket(3)
  iteration <- 1
  while(any(tg$run)) {
    message(paste0("Iteration ", iteration))
    source(file.path(here::here(), "scirep_charite", "03_run_grid.R"))
    source(file.path(here::here(), "scirep_charite", "04_eval_fs.R"))
    iteration <- iteration + 1
  }
  
  write_rds(tg, tg_file)
  parallelStop()
# }

.time_end <- Sys.time()
(dt <- difftime(.time_end, .time_start, units = "auto"))

rm(tg_file)