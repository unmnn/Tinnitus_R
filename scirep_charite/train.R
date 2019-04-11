# df_train: dataset
# run_id: id of run in tg
# algo: (mlr) name of classifier/regressor
# hyper: list of hyperparameter values
# prepro: preprocessing steps
# pre: prefix for output files
# iter: iteration number
# overwrite: should existing files be overwritten?  
run_classification <- function(df_train, run_id, algo, hyper, prepro, pre, iter, overwrite = TRUE) {
  # Set evaluation measures depending on prediction type (regression vs. classification)
  is_regression <- is.numeric(df_train$response) 
  if(is_regression) {
    task_prefix <- "regr"
    measures <- list(rmse, setAggregation(rmse, test.sd), arsq, setAggregation(arsq, test.sd))
  } else {
    task_prefix <- "classif"
    measures <- list(acc, setAggregation(acc, test.sd), 
                     kappa, setAggregation(kappa, test.sd))
    if(length(levels(df_train$response)) == 2) {
      measures <- c(list(auc, setAggregation(auc, test.sd)), measures)
    }
  }
  
  # Define file path for model that will be trained
  output_file <- file.path(here::here(), "scirep_charite", "output", "models", iter,
                           str_c(pre, "_", run_id, ".rds"))
  
  # If overwrite == FALSE and output_file already exists, then abort model training
  if(!overwrite) {
    # browser()
    if(length(dir(path = dirname(output_file), 
                  pattern = paste0("^", pre, "_", run_id))) > 0) {
      message(paste("Skipping run", run_id, "since model already exists."))
      return(invisible())
    }
  }
  
  # Make mlr regression/classification task
  if(is_regression) {
    task <- makeRegrTask(id = "tinnitus_charite", data = as.data.frame(df_train), target = "response")
  } else {
    if(length(levels(df_train$response)) == 2){
      task <- makeClassifTask(id = "tinnitus_charite", data = as.data.frame(df_train), target = "response",
                              positive = rev(levels(df_train$response))[1])
    } else {
      task <- makeClassifTask(id = "tinnitus_charite", data = as.data.frame(df_train), target = "response")
    }
  }
  
  # Make mlr learner 
  if(is_regression) {
    learner <- makeLearner(paste0(task_prefix, ".", algo), predict.type = "response", fix.factors.prediction = TRUE)
  } else {
    learner <- makeLearner(paste0(task_prefix, ".", algo), predict.type = "prob", fix.factors.prediction = TRUE)
  }
  
  # Preprocess wrapper
  if(!is.null(prepro)) {
    learner <- makePreprocWrapperCaret(learner, method = prepro)
  }
  
  # Set hyper params
  # if number of remaining features < mtry parameter, then remove illegal values
  if("mtry" %in% names(hyper)) hyper$mtry <- hyper$mtry[hyper$mtry < ncol(df_train)]
  ps <- makeParamSet(params = imap(hyper, ~ makeDiscreteParam(.y, values = .x)))
  
  ctrl <- makeTuneControlGrid()
  
  # Tune params with cross-validation
  set.seed(.config$seed)
  cv <- makeResampleDesc("CV", iters = 10, predict = "both")
  set.seed(.config$seed)
  lrn_cv <- tuneParams(learner, task = task, resampling = cv, par.set = ps, control = ctrl, 
                       measures = measures, show.info = TRUE)
  
  # Apply cv with optimal parameters
  set.seed(.config$seed)
  r <- resample(setHyperPars(learner, par.vals = lrn_cv$x), task, cv, measures = measures, models = TRUE, keep.pred = TRUE)
  
  # Train model on full dataset with optimal hyperparms
  lrn_final <- setHyperPars(learner, par.vals = lrn_cv$x)
  set.seed(.config$seed)
  model <- train(lrn_final, task)
  
  if(!dir.exists(dirname(output_file))) dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  write_rds(list(model = model, cv = lrn_cv, r = r, df = df_train), output_file)
  return(invisible())
}