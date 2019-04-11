# Sys.setenv(LANG = "en_US.UTF-8")

if(!"pacman" %in% installed.packages()) install.packages("pacman")

# required CRAN packages
install_cran <- {setdiff(c(
  "bestNormalize",
  "C50",
  "DT",
  "e1071",
  "extrafont",
  "ggalluvial",
  "ggridges",
  "glmnet",
  "here",
  "iml",
  "janitor",
  "kernlab",
  "knitr",
  "mlr",
  "nnet",
  "randomForest",
  "ranger",
  "rlang",
  "rpart",
  "rpart.plot",
  "RWeka",
  "tictoc",
  "tidyverse",
  "xgboost"
), installed.packages())}

if(length(install_cran) > 0) pacman::p_install(install_cran, character.only = TRUE)
rm(install_cran)

git_pcks <- c(
  "thomasp85/patchwork"
)

if(length(git_pcks) > 0) {
  names(git_pcks) <- gsub(".*/(.*)", "\\1", git_pcks)
  install_github <- setdiff(names(git_pcks), installed.packages())
  if(length(install_github) > 0) pacman::p_install_gh(git_pcks[install_github])
  rm(install_github)
}

rm(git_pcks)

# extrafont::font_import()
# extrafont::loadfonts("win", quiet = TRUE)

# tinnitus options
topt <- list()
topt$fbg_levels <- c("tq", "sozk", "tinskal", "acsa", "psq", "sf8", "swop", 
                     "tlq", "adsl", "bsf", "bi", "ses", "schmerzskal", "phqk", 
                     "isr", "soc" )
topt$console_width <- 90
options(width = topt$console_width)
options(str = strOptions(strict.width = "wrap"))

topt$mycols <- c(
  a_pink = "#f0027f",
  e_green = "#7fc97f",
  purple1 = "#F2F0F7",
  purple2 = "#CBC9E2",
  purple3 = "#9E9AC8",
  purple4 = "#756BB1",
  purple5 = "#54278F",
  grey = "grey50",
  red = "indianred3",
  blue = "steelblue3"
)

topt$algo <- c(
  baseline = "Baseline",
  lm = "Linear\nRegression",
  glmnet = "LASSO\nRegression",
  lasso = "lasso",
  ridge = "ridge",
  knn = "knn",
  kknn = "wknn",
  naiveBayes = "nb",
  svm = "svm",
  plsdaCaret = "gpls",
  nnet = "nnet",
  rpart = "cart",
  `C50` = "c5.0",
  rf = "rf",
  ranger = "rf",
  xgbTree = "gbt",
  xgboost = "gbt"
)

topt$depression_label <- c(
  r = "Depression-relevant",
  i = "Depression-irrelevant",
  n = "Depression-irrelevant"
)

topt$meta_cols <- c(
  ".jour_nr", ".age", ".testdatum", ".phase"
)

topt$pos_items <- c(
  "acsa_acsa100",
  "psq_freud100",
  "sf8_pcs8",
  "sf8_mcs8",
  "swop_opt100",
  "swop_sw100"
)

topt$base_family <- "sans"

# set ggplot2 theme
ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 11, base_family = topt$base_family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) + 
    ggplot2::theme(axis.line = ggplot2::element_line()) + 
    ggplot2::theme(axis.ticks = ggplot2::element_line()) +
    ggplot2::theme(axis.title = ggplot2::element_text(hjust = 1)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black"))
)


df <- readr::read_rds(file.path(here::here(), "data", "190311_data_as_tibble.rds"))
# data_list <- readr::read_rds(file.path(here::here(), "data", "190311_data_as_list.rds"))

source(file.path(here::here(), "Data", "data_dictionary.R"), encoding = "UTF-8")
source(file.path(here::here(), "scirep_charite", "helpers.R"), encoding = "UTF-8")

# Setup ----
.config <- list()
# name of response variable
.config$response <- "adsl_adsl_sum" #"tq_tf" 
# label for response (used for plots)
.config$response_label <- "depression status" # "tinnitus\nseverity" 
# whether to use the response@admission for modeling
.config$use_response <- FALSE
# model type: "classif" or "regr"
.config$model_type <- "classif"
# whether the response is the difference between the values at discharge and admission
.config$response_as_diff <- FALSE
# random seed
.config$seed <- 2019

# pre = prefix for output files such as figures
.config$pre <- .config$response
# dur: don't use response
if(.config$use_response) {.config$pre <- paste0(.config$pre, "-ur")} else {.config$pre <- paste0(.config$pre, "-dur")} 
.config$pre <- paste0(.config$pre, "-", .config$model_type)
# rnad: response not as difference (between E and A)
if(.config$response_as_diff) {.config$pre <- paste0(.config$pre, "-rad")} else {.config$pre <- paste0(.config$pre, "-rnad")} 

if(.config$response == "adsl_adsl_sum") {.config$questionnaires <- c("adsl", "psq", "sf8", "sozk", "tq", "tinskal", "tlq")}
if(.config$response == "tq_tf") {.config$questionnaires <- c("adsl", "psq", "sf8", "sozk", "tq", "tinskal", "tlq")}
