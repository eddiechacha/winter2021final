
library(tidymodels)
library(tidyverse)
library(lubridate)
library(kknn)
library(xgboost)
library(numbers)
set.seed(123)

#converting to dates
sp500_companies <- read_csv("data/unprocessed/all_stocks_5yr.csv")

sp500_total <- read_csv("data/unprocessed/data_csv.csv")

#dividing the dates into chunks

sp500_companies <- sp500_companies %>%
  mutate(date = ymd(date))

sp500_total <- sp500_total %>%
  mutate(date = ymd(Date)) 

write_rds(sp500_total, "data/processed/sp500_total.rds")
write_rds(sp500_companies, "data/processed/sp500_companies.rds")

company_names <- read_csv("data/unprocessed/constituents.csv")

#joining the companies data with the names data

sp500_companies <- inner_join(sp500_companies, company_names, 
                       c("Name" = "Symbol"))

#filtering out data that's not in our timespan

sp500_comp_compare <- sp500_companies %>%
  filter(year(date) == 2017) 

sp500_companies <- sp500_companies %>%
  filter(year(date) > 2012) %>%
  filter(year(date) < 2017)

sp500_total_compare <- sp500_total %>%
  filter(year(date) == 2017)

sp500_total <- sp500_total %>%
  filter(year(date) > 2012) %>%
  filter(year(date) < 2017)

write_rds(sp500_total, "data/processed/sp500_total.rds")
write_rds(sp500_companies, "data/processed/sp500_companies.rds")
write_rds(sp500_total_compare, "data/processed/sp500_total_compare.rds")
write_rds(sp500_comp_compare, "data/processed/sp500_comp_compare.rds")

#2013 DATA STARTS IN FEBRUARY - HAVE TO CHANGE DATA REGION  


## Setting up the model

lm_model <- linear_reg(mode = "regression") %>%
  set_engine("lm")

#I could do an elastic net model, BUT you don't have to

bt_model <- boost_tree(mode = "regression", trees = tune(), mtry = tune(), learn_rate = tune()) %>%
  set_engine("xgboost")
#tune mtry & learn_rate!!!

knn_model <- nearest_neighbor(mode = "regression", neighbors = tune()) %>%
  set_engine("kknn")

bt_params <- parameters(bt_model) %>%
  update(mtry = mtry(range = c(2,10)))
bt_grid <- grid_regular(bt_params, levels = 1)

knn_params <- parameters(knn_model)
knn_grid <- grid_regular(knn_params, levels = 1)

#This code generates a model for a (randomly selected) individual company and then presents the difference between the actual stock price in 2017 and the predicted model price.

comp_model <- function(name_of_company, name_of_model, prepped_grid = 5) {
  
comp_data_take2 <- sp500_companies %>%
  filter(Name == random_company) %>%
  filter(mod(week(date), 2) == 0) %>%
  filter(wday(date) == 2) %>%
  #Creating a variable for the percent change in volume from the previous snap
  mutate(volume_change = (volume/lag(volume) - 1) * 100) %>%
  #Creating a variable for the percent change in closing price from the previous snap
  mutate(price_change = (close/lag(close) - 1) * 100) %>%
  #Creating a variable for the percent range between highest and lowest price from the previous snap
  mutate(prev_hilo = (((lag(high)/lag(low)) - 1) * 100)) %>%
  #Creating a variable for the percent range between the open and closing price from the previous snap
  mutate(prev_openclose = (((lag(close)/lag(open)) - 1) * 100)) %>%
  select(-c(date, Name, Name.y, Sector, open, high, low, close, volume)) %>%
  na.omit(c(volume_change, price_change, prev_hilo, prev_openclose))

comp_split <- comp_data_take2 %>%
  initial_split(prop = .8, strata = price_change, na.rm = TRUE)

comp_train <- training(comp_split)

comp_test <- testing(comp_split)

comp_recipe <- recipe(price_change ~ ., data = comp_train) %>%
  #center and scaling
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

comp_workflow <- workflow() %>%
  add_recipe(comp_recipe) %>%
  add_model(name_of_model)

#folding the data and tuning the model
comp_folds <- vfold_cv(comp_train, v = 5, repeats = 3)

comp_tune <- comp_workflow %>% tune_grid(resamples = comp_folds, grid = prepped_grid)

best_model <- select_best(comp_tune, metric = "rmse")

#This is going to vary based on the model

#COMBINED
logistic_param <-
  tibble(
    trees = best_model$trees,
    neighbors = best_model$neighbors,
    mtry = best_model$mtry,
    learn_rate = best_model$learn_rate
  )

final_workflow <- 
  comp_workflow %>% 
  finalize_workflow(logistic_param)

#either tune based on resamples or pick best model and put that into fit
comp_fit <- fit(final_workflow, comp_train)

#building a test set

comp_truth <- sp500_comp_compare %>%
  filter(Name == random_company) %>%
  filter(mod(week(date), 2) == 0) %>%
  filter(wday(date) == 2) %>%
  #Creating a variable for the percent change in volume from the previous snap
  mutate(volume_change = (volume/lag(volume) - 1) * 100) %>%
  #Creating a variable for the percent range between highest and lowest price from the previous snap
  mutate(prev_hilo = (((lag(high)/lag(low)) - 1) * 100)) %>%
  #Creating a variable for the percent range between the open and closing price from the previous snap
  mutate(prev_openclose = (((lag(close)/lag(open)) - 1) * 100)) %>%
  select(-c(date, Name, Name.y, Sector, open, high, low, close, volume))

comp_truth <- na.omit(comp_truth)

true_price_change <- sp500_comp_compare %>%
  filter(Name == random_company) %>%
  filter(mod(week(date), 2) == 0) %>%
  filter(wday(date) == 2) %>%
  #Creating a variable for the percent change in closing price from the previous snap
  mutate(price_change = (close/lag(close) - 1) * 100)

true_price_change <- na.omit(true_price_change)

comp_predicted <- predict(comp_fit, comp_truth)

comp_results <- data.frame(true_price_change$price_change, comp_predicted,(((true_price_change$price_change/comp_predicted) - 1)))

comp_results <- comp_results %>% rename("original" = true_price_change.price_change, "predicted" = .pred, "difference" = .pred.1)

#combining all the data back onto one data frame to graph it
comp_summary <- inner_join(true_price_change, comp_results, c("price_change" = "original"))

#make the differences absolute values, or square difference and root mean to do RMSE

avg_difference <- mean(abs(comp_summary$difference), na.rm = TRUE)

}

random_company_list <- as.list(unique(sp500_companies$Name))
model_list <- c("lm_model", "bt_model", "knn_model")
compare_models <- data.frame(0, "Company name", "Model")

#generating all the models
for (c in random_company_list) {
  random_company = c
  for (m in model_list) {
    if (m == "lm_model") {
      comp_model(c, lm_model)
      compare_models[nrow(compare_models) + 1,] = c(comp_model(c, lm_model), random_company, "Linear regression")
    }
    if (m == "bt_model") {
      comp_model(c, bt_model, bt_grid)
      compare_models[nrow(compare_models) + 1,] = c(comp_model(c, bt_model), random_company, "Boosted tree")
    }
    if (m == "knn_model") {
      comp_model(c, knn_model, knn_grid)
      compare_models[nrow(compare_models) + 1,] = c(comp_model(c, knn_model), random_company, "Nearest neighbors model")
    }
  }
}

write_rds(compare_models, "data/processed/compare_models.rds")

#if you're going to offload stuff, make sure everything above has already ran
#save all those files to the .zip!

#example of saving code:
#save(compare_models, company_names, file = "data/model_results.rda")

#load(file = "data/bt_tune.rda")