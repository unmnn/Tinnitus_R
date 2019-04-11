# Helper functions ----
library("tidyverse")
library("rlang")

# discretizes response variable
# x: numerical vector. If y is not NULL, then admission values
# y: numerical vector (optional). Discharge values
# response: response name as string
# as_diff: mode
discretize_response <- function(x, y = NULL, response, as_diff) {
  if(response == "adsl_adsl_sum") {
    # s: subclinical depression; c: clinical depression
    if(as_diff == FALSE) {
      o <- cut(x, breaks = c(-Inf, 15, Inf), labels = c("s", "c"))
    } else {
      o <- vector("character", length(x))
      o[x <= 15 & y <= 15] <- "ss"
      o[x <= 15 & y > 15] <- "sc"
      o[x > 15 & y <= 15] <- "cs"
      o[x > 15 & y > 15] <- "cc"
      o <- as.factor(o)
    }
  }
  if(response == "tq_tf") {
    # c: compensated tinnitus; d: decompensated tinnitus
    if(as_diff == FALSE) {
      o <- cut(x, breaks = c(-Inf, 46, Inf), labels = c("c", "d"))
    } else {
      stop("Currently not implemented.")
    }
  }
  return(o)
}

# Prepare training data
# Filter patients that fullfil the following requirements:
# - exhibit recordings at 'A' and 'E'
# - 1st recording is in phase 'A'
# - 2nd recording is in phase 'E'
# output is a data frame with two rows for each patient
# df: data frame
# additional_q: character vector of names of questionnaires additional to .config$questionnaires
prepare_train <- function(df, additional_q = NULL) {
  dfr <- df %>%
    arrange(.jour_nr, .testdatum) %>%
    group_by(.jour_nr) %>%
    mutate(.phase_course = str_c(.phase, collapse = "")) %>%
    add_count() %>%
    filter(n > 1) %>%
    mutate(valid = if_else(.phase[1] == "A" & .phase[2] == "E", TRUE, FALSE)) %>%
    ungroup() %>%
    filter(valid) %>%
    group_by(.jour_nr) %>%
    slice(1:2) %>%
    ungroup() %>%
    group_by(.jour_nr) %>%
    mutate(valid = all(!is.na(!!sym(.config$response)))) %>%
    ungroup() %>%
    filter(valid) %>%
    select(-c(valid, n, .phase_course))
  
  # Define response
  # For response, make the value in 'E' the target value
  # Remove other values of 'E' so that only recordings in 'A' and response in 
  # 'E' are available 
  dfr <- dfr %>%
    group_by(.jour_nr) %>%
    mutate(response = if_else(.config$response_as_diff,
                              (!!sym(.config$response))[2] - 
                                (!!sym(.config$response))[1],
                              (!!sym(.config$response))[2])) %>%
    mutate(response_admission = (!!sym(.config$response))[1]) %>%
    slice(1) %>%
    ungroup()
  
  # Filter questionnaires ----
  df_train <- dfr %>%
    select(-.phase) %>%
    extract_date_features2() %>%
    select(starts_with("."), 
           starts_with("response"), 
           matches(str_c("^(", str_c(.config$questionnaires, 
                                     collapse = "|"), ").*$"))) %>%
    # remove response_org and derivates
    # select(-matches(paste0(.config$response, ".+"))) %>% 
    select(-.testdatum)
  
  # Handle missing values ----
  # ratio of missing values per patient
  rat_missing <- apply(df_train %>% select(-.jour_nr), 1, function(x) mean(is.na(x)))
  # mean(rat_missing > 0.05)
  
  # remove patients with more than 5% missing values
  # for other patients, apply na.roughfix
  df_train <- df_train %>% filter(!rat_missing > 0.05) %>%
    map_dfc(randomForest::na.roughfix)
  
  if(!is.null(additional_q)) {
    df_train <- df_train %>%
      inner_join(dfr %>% select(.jour_nr, matches(str_c("^(", str_c(additional_q, collapse = "|"), ").*$"))), by = ".jour_nr")
  }
  
  return(df_train)
}

# Makes questionnaire abbrev. prefix in variables names UPPERCASE
qname_to_upper <- function(char) {
  s <- str_split(char, "_", n = 2)
  map_chr(s, function(x){
    if(length(x) == 1) {
      x
    } else {
      paste0(str_to_upper(x[1]), "_", x[2])
    }
  })
}

# Extract temporal features from '.testdatum'
extract_date_features2 <- function(df) {
  df %>%
    mutate(.day = lubridate::day(.testdatum)) %>%
    mutate(.weekday = lubridate::wday(.testdatum, week_start = 1)) %>%
    mutate(.yearday = lubridate::yday(.testdatum)) %>%
    mutate(.week = lubridate::week(.testdatum)) %>%
    mutate(.month = lubridate::month(.testdatum)) 
}

# Get "publication-ready" levels for response
full_levels_response <- function(response, short = FALSE) {
  if(response == "adsl_adsl_sum") {
    if(short) {
      out <- c("subclinical", "clinical")
    } else {
      out <- c("subclinical depression", "clinical depression")
    }
  }
  
  if(response == "tq_tf") {
    if(short) {
      out <- c("compensated", "decompensated")
    } else {
      out <- c("compensated tinnitus", "decompensated tinnitus")
    }
  }
  return(out)
}