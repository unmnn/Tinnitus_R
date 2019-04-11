for(i in 1:nrow(tg)) {
  if(tg$run[[i]] == TRUE) {
    if(iteration > 1 & !is.null(tg$models[[i]])) { # the latter occurs e.g. when lm is used on a classfication task
      df_imp <- tg$models[[i]]$imp[iteration - 1][[1]]
      # filter features with MR score > 1
      feat <- filter(df_imp, importance > 1) %>% pull(feature)
      dft <- select(df_train, response, feat)
    } else {
      dft <- df_train
    }
    
    try({
      res <- run_classification(dft, run_id = tg$id[i], algo= tg$algo[i], 
                                hyper = tg$hyper[[i]], prepro = tg$prepro[[i]], 
                                pre = .config$pre, iter = iteration,
                                overwrite = FALSE)
    })
    
    # terminate if there was no model trained
    if(exists("res")){
      rm(res)
    } else {
      tg$run[[i]] <- FALSE
    }
  }
}


