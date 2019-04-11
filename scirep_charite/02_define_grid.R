# Create hyperparameter optimization grid
tg <- tribble(
  ~algo, ~hyper, ~prepro, 
  # "lm", list(tol = 1e-07), list(c("center", "scale")), # list(NULL, c("center", "scale"), "pca", c("center", "scale", "corr")),
  # LASSO
  "glmnet", list(alpha = 1, lambda = 10^seq(10, -2, length = 100)), list(NULL), # list(NULL, "corr", "pca"),
  # RIDGE
  "glmnet", list(alpha = 0, lambda = 10^seq(10, -2, length = 100)), list(NULL), #ridge
  # Weighted knn
  "kknn", list(k = seq(from = 1, by = 4, length.out = 20)), list(NULL), # list(NULL, "range", c("center", "scale"), "pca", c("center", "scale", "corr")),
  # SVM
  "svm", list(cost = c(0.01, 0.1, 0.5, 1:3), gamma = 0:3,
              kernel = c("linear", "polynomial", "radial", "sigmoid")), list(NULL),
  # Naive Bayes
  "naiveBayes", list(laplace = 1:5), list(NULL),
  # "lssvm", list(kernel = c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot"), tau = c(0.01, 0.1, 0.5, 1:3)), list(NULL),
  # Neural Net with one single hidden layer
  "nnet", list(size = seq(1,13,2), decay = 10^-(seq(4,1)), MaxNWts = 100000), list(NULL),
  # CART decision tree
  "rpart", list(cp = c(0.001, 0.005, 0.01, 0.05, 0.1)), list(NULL),
  # C5.0 decision tree
  "C50", list(winnow = c(winnow_false = FALSE, winnow_true = TRUE),
              CF = seq(0,0.35,0.05),
              rules = c(rules_false = FALSE, rules_true = TRUE)), list(NULL),
  # Partial least squares
  "plsdaCaret", list(ncomp = 1:5), list(NULL),
  # Random forest
  "randomForest", list(mtry = seq(4, min(100, ncol(df_train)-1), 16), ntree = seq(100, 900, 200)), list(NULL),
  "ranger", list(mtry = seq(4, min(100, ncol(df_train)-1), 16), min.node.size = c(1, seq(5,25,5))), list(NULL), # splitrule = c("variance"),
  # Gradient boosted trees
  "xgboost", list(eta = c(0.01, 0.05, 0.1, 0.2), max_depth = 1:3, gamma = 0, colsample_bytree = seq(0.2, 1, 0.2),
                  min_child_weight = c(0.5, 1, 2), subsample = seq(0.5, 1, 0.25), nrounds = seq(50, 250, 100)), list(NULL)
) %>%
  unnest(prepro, .drop = FALSE) %>%
  mutate(id = str_pad(row_number(), width = 2, pad = "0")) %>%
  select(id, everything()) %>%
  mutate(models = vector("list", n())) %>%
  mutate(run = TRUE)

# getCaretParamSet("naive_bayes", task = task, length = 5)
# mlr::getLearnerParamSet("classif.xgboost")
# mlrHyperopt::getDefaultParConfig(makeLearner("classif.ksvm"))
# # List of integrated learners https://mlr.mlr-org.com/articles/tutorial/integrated_learners.html
# getParamSet("classif.randomForest")