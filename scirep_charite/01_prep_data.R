df_train <- prepare_train(df)

# Prior to any discretization, make a copy of the response.
# This variable will be dropped prior to training, but will be saved alongside 
# the other variables for plotting of the original distribution later on.
df_train <- df_train %>% mutate(response_org = response)

if(.config$model_type == "classif") {
  df_train$response <- discretize_response(df_train$response, y = NULL, 
                                           response = .config$response, 
                                           as_diff = .config$response_as_diff)
}

if(.config$use_response == FALSE) df_train <- df_train %>% select(-!!.config$response)

file_path <- file.path(here::here(), "scirep_charite", "output", "data", paste0(.config$pre, "_df.rds"))
if(!dir.exists(dirname(file_path))) dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
write_rds(df_train, file_path)

df_train <- df_train %>% select(-ends_with("_org"))