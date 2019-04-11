file_path_models <- file.path(here::here(), "scirep_charite", "output", "models", iteration)
file_path_imp <- file.path(here::here(), "scirep_charite", "output", "imp", iteration)
if(!dir.exists(file_path_imp)) dir.create(file_path_imp, recursive = TRUE, showWarnings = FALSE)

files_models <- dir(file_path_models, pattern = .config$pre, full.names = TRUE)

# iterates over all models of the current iteration and calculates MR
# also adds resampling, cross-validation, model objects to tg (tg$models) 
for(i in seq_along(files_models)) {
  rds <- read_rds(files_models[i])
  idx <- str_replace(files_models[i], ".*_(\\d{2})\\.rds$", "\\1")
  file_imp <- file.path(file_path_imp, str_c(.config$pre, "_", idx, ".rds"))
  if(!file.exists(file_imp)) {
    predictor <- Predictor$new(model = rds$model,
                               data = rds$df, 
                               y = "response")
    set.seed(.config$seed)
    imp <- FeatureImp$new(predictor, 
                          loss = if_else(.config$model_type == "regr", "rmse", "ce"),
                          n.repetitions = 10) 
    write_rds(imp, file_imp)
    
    rm(predictor)
    # table(c("i", "r")[apply(predict(rds$model$learner.model, df_train, type = "response")$predictions, 1, which.max)], df_train$response)
    
  } else {
    imp <- read_rds(file_imp)
  }
  
  if(imp$original.error == 0) { # actually, I don't know why we need to do this
    imp$results$importance <- imp$results$importance + 1
  }
  
    tg$models[[as.integer(idx)]] <- bind_rows(
      tg$models[[as.integer(idx)]], 
      tibble(iter = iteration, 
             model = list(rds$model), 
             cv = list(rds$cv), 
             r = list(rds$r), 
             imp = list(imp$results)))
  
    # if there is something strange with the MR values or...
    # ...all MR values are above 1 or...
    # ...all MR values are below 1 THEN terminate run
    if(any(is.infinite(imp$results$importance) | is.nan(imp$results$importance))) {
      tg$run[[as.integer(idx)]] <- FALSE
    } else {
      if(min(imp$results$importance) > 1 | max(imp$results$importance) < 1) tg$run[[as.integer(idx)]] <- FALSE
    }
}

rm(file_path_models, file_path_imp, files_models, i, rds, idx, file_imp, imp)

# map(tg$models, function(x) {map_int(x$imp, ~nrow(.x))})
# map(tg$models, function(x) {map_dbl(x$r, ~.x$aggr[[1]])}) 