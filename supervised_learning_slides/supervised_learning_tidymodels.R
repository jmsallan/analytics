## ----setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)
library(kableExtra)


## ---- out.width='60%', fig.align='center'----------------------------------
knitr::include_graphics('image/spamFilter.jpg')


## ---- out.width='50%', fig.align='center'----------------------------------
knitr::include_graphics('image/housing.png')


## ---- out.width='60%', fig.align='center', echo=FALSE----------------------
knitr::include_graphics('image/biasvariance.png')


## ---- out.width='90%', fig.align='center', echo=FALSE----------------------
knitr::include_graphics('image/MLstages.png')


## ---- out.width='90%', fig.align='center'----------------------------------
knitr::include_graphics('image/tidyflow.png')


## ---- out.width='90%', fig.align='center'----------------------------------
knitr::include_graphics('image/tidymodels.png')


## --------------------------------------------------------------------------
library(tidymodels)


## --------------------------------------------------------------------------
iris %>% glimpse()


## --------------------------------------------------------------------------
set.seed(1212)
iris_split <- initial_split(iris, prop = 0.6)


## --------------------------------------------------------------------------
training(iris_split) %>% glimpse() #train set


## --------------------------------------------------------------------------
testing(iris_split) %>% glimpse() # test set


## --------------------------------------------------------------------------
iris_recipe <- training(iris_split) %>%
  recipe(Species ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()


## --------------------------------------------------------------------------
iris_recipe


## --------------------------------------------------------------------------
iris_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger")


## --------------------------------------------------------------------------
iris_ranger_wf <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(iris_ranger) %>%
  fit(training(iris_split))


## --------------------------------------------------------------------------
iris_ranger_wf


## --------------------------------------------------------------------------
iris_pred_train <- iris_ranger_wf %>%
  predict(training(iris_split)) %>%
  bind_cols(training(iris_split))
iris_pred_train %>% glimpse()


## --------------------------------------------------------------------------
iris_pred_train %>%
  conf_mat(truth = Species, estimate = .pred_class)


## ---- echo=FALSE-----------------------------------------------------------
cm_table <- data.frame(positive = c(100, 200), negative = c(300, 3000))
rownames(cm_table) <- c("positive", "negative")
kbl(cm_table) %>%
    kable_styling(bootstrap_options = "striped", full_width = F, position = "center")


## --------------------------------------------------------------------------
class_metrics <- metric_set(accuracy, precision, recall)


## --------------------------------------------------------------------------
iris_pred_train %>%
  class_metrics(truth = Species, estimate = .pred_class)


## --------------------------------------------------------------------------
iris_prob_train <- iris_ranger_wf %>%
  predict(training(iris_split), type = "prob") %>%
  bind_cols(training(iris_split))
iris_prob_train %>% glimpse()


## --------------------------------------------------------------------------
iris_prob_train %>%
  roc_auc(Species, .pred_setosa:.pred_virginica)


## --------------------------------------------------------------------------
iris_prob_train %>%
  roc_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()


## --------------------------------------------------------------------------
iris_ranger_wf %>%
  predict(testing(iris_split)) %>%
  bind_cols(testing(iris_split)) %>%
  conf_mat(truth = Species, estimate = .pred_class)


## --------------------------------------------------------------------------
iris_ranger_wf %>%
  predict(testing(iris_split)) %>%
  bind_cols(testing(iris_split)) %>%
  class_metrics(truth = Species, estimate = .pred_class)


## --------------------------------------------------------------------------
library(mlbench)
data("BostonHousing")
BostonHousing %>% glimpse()


## --------------------------------------------------------------------------
set.seed(2021)
bh_split <- initial_split(BostonHousing, prob = 0.8)


## --------------------------------------------------------------------------
bh_recipe <- training(bh_split) %>%
  recipe(medv ~ .) %>%
  step_mutate(chas = as.numeric(chas)) %>%
  step_corr(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  prep()


## --------------------------------------------------------------------------
bh_recipe


## --------------------------------------------------------------------------
bh_lm <- linear_reg("regression") %>%
  set_engine("lm")


## --------------------------------------------------------------------------
bh_lm_wf <- workflow() %>%
  add_recipe(bh_recipe) %>%
  add_model(bh_lm) %>%
  fit(training(bh_split))


## --------------------------------------------------------------------------
bh_lm_wf


## --------------------------------------------------------------------------
bh_lm_wf_pred <- bh_lm_wf %>%
  predict(training(bh_split)) %>%
  bind_cols(training(bh_split))


## --------------------------------------------------------------------------
bh_lm_wf_pred %>%
  select(medv, .pred) %>%
  glimpse()


## --------------------------------------------------------------------------
bh_lm_wf_pred %>%
  metrics(truth = medv, estimate = .pred)


## --------------------------------------------------------------------------
bh_lm_wf %>%
  predict(testing(bh_split)) %>%
  bind_cols(testing(bh_split)) %>%
  metrics(truth = medv, estimate = .pred)


## ---- out.width='80%', fig.align='center', echo=FALSE----------------------
knitr::include_graphics('image/resampling.svg')


## --------------------------------------------------------------------------
bh_wf <- workflow() %>%
  add_recipe(bh_recipe) %>%
  add_model(bh_lm)


## --------------------------------------------------------------------------
set.seed(1212)
bh_folds <- vfold_cv(training(bh_split), v = 5)


## --------------------------------------------------------------------------
fit_resamples(bh_wf, bh_folds) %>%
  collect_metrics()


## --------------------------------------------------------------------------
rf_grid <- expand.grid(mtry = c(1, 5, 10), trees = c(5, 10, 15))
rf_grid


## --------------------------------------------------------------------------
bh_rf <- rand_forest(mode = "regression", mtry = tune(), trees = tune()) %>%
  set_engine("ranger")


## ---- cache=TRUE-----------------------------------------------------------
rf_tune <- tune_grid(object = bh_rf, 
                     preprocessor = bh_recipe, 
                     resamples = bh_folds, 
                     grid = rf_grid, 
                     metrics = metric_set(rmse, mae))


## --------------------------------------------------------------------------
show_best(rf_tune, metric = "mae")


## --------------------------------------------------------------------------
imbal_data <- 
  readr::read_csv("https://bit.ly/imbal_data") %>% 
  mutate(Class = factor(Class))


## ---- echo=FALSE-----------------------------------------------------------
imbal_data %>%
  ggplot(aes(Class)) +
  geom_bar(stat = "count")


## --------------------------------------------------------------------------
set.seed(1122)
imbal_split <- initial_split(imbal_data, prop = 0.8, strata = "Class")


## --------------------------------------------------------------------------
library(discrim)
qda_mod <- discrim_regularized(frac_common_cov = 0, frac_identity = 0) %>%  
  set_engine("klaR")


## --------------------------------------------------------------------------
qda_wflw <- 
  workflow() %>% 
  add_model(qda_mod) %>% 
  add_formula(Class ~ .) %>%
  fit(training(imbal_split))


## --------------------------------------------------------------------------
set.seed(5732)
cv_folds <- vfold_cv(training(imbal_split), 
                     strata = "Class", 
                     v=5, repeats = 5)


## --------------------------------------------------------------------------
cls_metrics <- metric_set(accuracy, sens, spec)


## ---- cache=TRUE-----------------------------------------------------------
set.seed(2180)
qda_res <- fit_resamples(
  qda_wflw, 
  resamples = cv_folds, 
  metrics = cls_metrics)


## --------------------------------------------------------------------------
collect_metrics(qda_res)


## --------------------------------------------------------------------------
library(themis)
imbal_rec <- 
  recipe(Class ~ ., data = imbal_data) %>%
  step_rose(Class)


## --------------------------------------------------------------------------
qda_rose_wflw <- 
  workflow() %>% 
  add_model(qda_mod) %>% 
  add_recipe(imbal_rec)


## ---- cache=TRUE-----------------------------------------------------------
set.seed(2180)
qda_rose_res <- fit_resamples(
  qda_rose_wflw, 
  resamples = cv_folds, 
  metrics = cls_metrics)


## --------------------------------------------------------------------------
collect_metrics(qda_res)


## --------------------------------------------------------------------------
collect_metrics(qda_rose_res)


## --------------------------------------------------------------------------
fit_rose <- qda_rose_wflw %>%
  fit(training(imbal_split))


## --------------------------------------------------------------------------
predict_rose_train <- fit_rose %>%
    predict(training(imbal_split)) %>%
    bind_cols(training(imbal_split))


## --------------------------------------------------------------------------
predict_rose_test <- fit_rose %>%
    predict(testing(imbal_split)) %>%
    bind_cols(testing(imbal_split))


## --------------------------------------------------------------------------
predict_rose_train %>%
    conf_mat(truth = Class, estimate = .pred_class)


## --------------------------------------------------------------------------
predict_rose_test %>%
    conf_mat(truth = Class, estimate = .pred_class)


## --------------------------------------------------------------------------
cls_metrics <- metric_set(accuracy, sens, spec)


## --------------------------------------------------------------------------
predict_rose_train %>%
  cls_metrics(truth = Class, estimate = .pred_class)


## --------------------------------------------------------------------------
predict_rose_test %>%
  cls_metrics(truth = Class, estimate = .pred_class)

