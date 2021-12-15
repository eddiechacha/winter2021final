sp500_companies <- sp500_companies %>%
  #date = as.Date(date) %>%
  mutate(date = ymd(date))

head(ymd(sp500_companies$date))

str(sp500_companies)

ggplot(data = sp500_companies, aes(x = close)) +
  geom_density() +
  facet_wrap(~ Sector.x)

print(sp500_total_compare$SP500)
print(sp500_predicted)

sp_results <- data.frame(sp500_total_compare$SP500, sp500_predicted, (sp500_total_compare$SP500 - sp500_predicted))
sp_results <- sp_results %>% rename("original" = sp500_total_compare.SP500, "predicted" = .pred, "difference" = .pred.1)

print(sp_results)
print(sp_results$original)

library(janitor)

#EXPERIMENT: Creating a list of all the companies and to then run the model through
random_company_list <- as.list(sample(sp500_companies$Name, 500))



#combining all the data back onto one data frame to graph it
sp_summary <- inner_join(sp500_total_compare, sp_results, c("SP500" = "original"))

ggplot(sp_summary)+
  geom_line(aes(month, difference, group = 1))

random_company <- sample(sp500_companies$Name, 1)

comp_data <- sp500_companies %>%
  filter(Name == random_company) %>%
  select(-c(year, month, day, Name, Name.y, Sector))

comp_split <- comp_data %>%
  initial_split(prop = .8, strata = close)

comp_train <- training(comp_split)

comp_test <- testing(comp_split)

comp_recipe <- recipe(close ~ ., data = comp_train) %>%
#center and scaling
step_center(all_predictors()) %>%
step_scale(all_predictors())

#testing it out with prep (we would normally use fit)
prep(comp_recipe) %>%
  juice()

comp_workflow <- workflow() %>%
  add_recipe(comp_recipe) %>%
  add_model(lm_model)

comp_fit <- fit(comp_workflow, comp_train)

comp_truth <- sp500_comp_compare %>% filter(Name = random_company) %>% select(-c(Name, Name.y, Sector, month, day, year))

sp500_predicted <- predict(sp_fit, sp500_truth)

library(numbers)

random_company <- sample(sp500_companies$Name, 1)
print(random_company)

comp_data_take2 <- sp500_companies %>%
  filter(Name == random_company) %>%
  #filter(day(date) == 7 | day(date) == 21) %>%
  #i could also use epiweek() or isoweek() for different week interpretations
  filter(mod(week(date), 2) == 0) %>%
  filter(wday(date) == 2) %>%
  #group_by(year(date)) %>%
  #group_by(week(date)) %>%
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

#testing it out with prep (we would normally use fit)
#prep(comp_recipe) %>%
#  juice()

comp_workflow <- workflow() %>%
  add_recipe(comp_recipe) %>%
  add_model(lm_model)

#folding the data
#comp_folds <- vfold_cv(comp_train, v = 5, repeats = 3)

#comp_fit <- fit_resamples(comp_workflow, comp_folds)

comp_fit <- fit(comp_workflow, comp_train)

comp_truth <- sp500_comp_compare %>%
  filter(Name == random_company) %>%
  #filter(day(date) == 7 | day(date) == 21) %>%
  #i could also use epiweek() or isoweek() for different week interpretations
  filter(mod(week(date), 2) == 0) %>%
  filter(wday(date) == 2) %>%
  #group_by(year(date)) %>%
  #group_by(week(date)) %>%
  #Creating a variable for the percent change in volume from the previous snap
  mutate(volume_change = (volume/lag(volume) - 1) * 100) %>%
  #Creating a variable for the percent range between highest and lowest price from the previous snap
  mutate(prev_hilo = (((lag(high)/lag(low)) - 1) * 100)) %>%
  #Creating a variable for the percent range between the open and closing price from the previous snap
  mutate(prev_openclose = (((lag(close)/lag(open)) - 1) * 100)) %>%
  select(-c(date, Name, Name.y, Sector, open, high, low, close, volume))
  

true_price_change <- sp500_comp_compare %>%
  filter(Name == random_company) %>%
  #filter(day(date) == 7 | day(date) == 21) %>%
  #i could also use epiweek() or isoweek() for different week interpretations
  filter(mod(week(date), 2) == 0) %>%
  filter(wday(date) == 2) %>%
  #group_by(year(date)) %>%
  #group_by(week(date)) %>%
  #Creating a variable for the percent change in closing price from the previous snap
  mutate(price_change = (close/lag(close) - 1) * 100)

comp_predicted <- predict(comp_fit, comp_truth)

comp_results <- data.frame(true_price_change$price_change, comp_predicted,(((true_price_change$price_change/comp_predicted) - 1)))

#volume, high and low aren't knowable before the close happens
#use snapshots in time for the individual company data
#get the percent change in the dataset instead (14 day change)
#realign closing price with previous day information
#sketch out dataset: what should be here?
#narrow question - focus on percent change ACROSS models instead of focusing on ranking (unless for the final model)

comp_results <- comp_results %>% rename("original" = true_price_change.price_change, "predicted" = .pred, "difference" = .pred.1)

#combining all the data back onto one data frame to graph it
comp_summary <- inner_join(true_price_change, comp_results, c("price_change" = "original"))

#print(comp_results)

ggplot(comp_summary)+
  geom_line(aes(date, difference)) +
  labs(title = comp_summary$Name.y)

#FAILED MODEL

ranking <- tibble(x = 0, y = 0)

failed_comp_model <- function(random_company) {
  
  comp_data <- sp500_companies %>%
    filter(Name == random_company) %>%
    select(-c(date, Name, Name.y, Sector)) %>%
    mutate(fourteen_day = percentChange(close))
  
  comp_split <- comp_data %>%
    initial_split(prop = .8, strata = close)
  
  comp_train <- training(comp_split)
  
  comp_test <- testing(comp_split)
  
  comp_recipe <- recipe(close ~ ., data = comp_train) %>%
    #center and scaling
    step_center(all_predictors()) %>%
    step_scale(all_predictors())
  
  #testing it out with prep (we would normally use fit)
  #prep(comp_recipe) %>%
  #  juice()
  
  comp_workflow <- workflow() %>%
    add_recipe(comp_recipe) %>%
    add_model(lm_model)
  
  #folding the data
  #comp_folds <- vfold_cv(comp_train, v = 5, repeats = 3)
  
  #comp_fit <- fit_resamples(comp_workflow, comp_folds)
  
  comp_fit <- fit(comp_workflow, comp_train)
  
  comp_truth <- sp500_comp_compare %>% filter(Name == random_company) %>% select(-c(Name, Name.y, Sector, date, close))
  
  comp_data <- sp500_comp_compare %>% filter(Name == random_company)
  
  comp_predicted <- predict(comp_fit, comp_truth)
  
  #comp_results <- data.frame(comp_data$close, comp_predicted, (comp_data$close - comp_predicted))
  
  comp_results <- data.frame(comp_data$close, comp_predicted, (1 - (comp_data$close/comp_predicted)))
  
  #volume, high and low aren't knowable before the close happens
  #use snapshots in time for the individual company data
  #get the percent change in the dataset instead (14 day change)
  #realign closing price with previous day information
  #sketch out dataset: what should be here?
  #narrow question - focus on percent change ACROSS models instead of focusing on ranking (unless for the final model)
  
  comp_results <- comp_results %>% rename("original" = comp_data.close, "predicted" = .pred, "difference" = .pred.1)
  
  #combining all the data back onto one data frame to graph it
  comp_summary <- inner_join(comp_data, comp_results, c("close" = "original"))
  
  #print(comp_results)
  
  ggplot(comp_summary)+
    geom_line(aes(date, difference)) +
    labs(title = comp_summary$Name.y)
}

#Re-orienting the model

knn_model <- nearest_neighbor() %>%
  set_engine("kknn")

bt_model <- boosted_tree() %>%
  set_engine("xgboost")

#bt_comp_model <- function(random_company) {
  
  comp_data_take2 <- sp500_companies %>%
    filter(Name == random_company) %>%
    #filter(day(date) == 7 | day(date) == 21) %>%
    #i could also use epiweek() or isoweek() for different week interpretations
    filter(mod(week(date), 2) == 0) %>%
    filter(wday(date) == 2) %>%
    #group_by(year(date)) %>%
    #group_by(week(date)) %>%
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
  
  #testing it out with prep (we would normally use fit)
  #prep(comp_recipe) %>%
  #  juice()
  
  comp_workflow <- workflow() %>%
    add_recipe(comp_recipe) %>%
    add_model(bt_model)
  
  keep_pred <- control_resamples(save_pred = TRUE)
  
  #folding the data
  comp_folds <- vfold_cv(comp_train, v = 10, repeats = 5)
  
  comp_fit <- fit_resamples(comp_workflow, comp_folds)
  #comp_fit <- fit_resamples(comp_workflow, resamples = comp_folds, control = keep_pred)
  
  comp_metrics <- collect_metrics(comp_fit)
  
  select_best(comp_fit, metric = "rmse")
  
  #comp_fit <- fit(comp_workflow, comp_train)
  
  comp_truth <- sp500_comp_compare %>%
    filter(Name == random_company) %>%
    #filter(day(date) == 7 | day(date) == 21) %>%
    #i could also use epiweek() or isoweek() for different week interpretations
    filter(mod(week(date), 2) == 0) %>%
    filter(wday(date) == 2) %>%
    #group_by(year(date)) %>%
    #group_by(week(date)) %>%
    #Creating a variable for the percent change in volume from the previous snap
    mutate(volume_change = (volume/lag(volume) - 1) * 100) %>%
    #Creating a variable for the percent range between highest and lowest price from the previous snap
    mutate(prev_hilo = (((lag(high)/lag(low)) - 1) * 100)) %>%
    #Creating a variable for the percent range between the open and closing price from the previous snap
    mutate(prev_openclose = (((lag(close)/lag(open)) - 1) * 100)) %>%
    select(-c(date, Name, Name.y, Sector, open, high, low, close, volume))
  
  
  true_price_change <- sp500_comp_compare %>%
    filter(Name == random_company) %>%
    #filter(day(date) == 7 | day(date) == 21) %>%
    #i could also use epiweek() or isoweek() for different week interpretations
    filter(mod(week(date), 2) == 0) %>%
    filter(wday(date) == 2) %>%
    #group_by(year(date)) %>%
    #group_by(week(date)) %>%
    #Creating a variable for the percent change in closing price from the previous snap
    mutate(price_change = (close/lag(close) - 1) * 100) %>%
  #THIS LINE ADDED FOR NEAREST NEIGHBOR MODEL
  filter(!is.na(price_change))
  
  comp_predicted <- predict(comp_fit, comp_truth) %>%
  #THIS LINE ADDED FOR NEAREST NEIGHBOR MODEL
  filter(!is.na(price_change))
  #comp_predicted <- predict(comp_fit, comp_test)
  
  comp_results <- data.frame(true_price_change$price_change, comp_predicted,(((true_price_change$price_change/comp_predicted) - 1)))
  
  #volume, high and low aren't knowable before the close happens
  #use snapshots in time for the individual company data
  #get the percent change in the dataset instead (14 day change)
  #realign closing price with previous day information
  #sketch out dataset: what should be here?
  #narrow question - focus on percent change ACROSS models instead of focusing on ranking (unless for the final model)
  
  comp_results <- comp_results %>% rename("original" = true_price_change.price_change, "predicted" = .pred, "difference" = .pred.1)
  
  #combining all the data back onto one data frame to graph it
  comp_summary <- inner_join(true_price_change, comp_results, c("price_change" = "original"))
  
  #print(comp_results)
  
  #ggplot(comp_summary)+
  #  geom_line(aes(date, difference)) #+
  #  labs(title = comp_summary$Name.y)
  
  ggplot(comp_summary)+
    geom_line(aes(date, price_change), color = "red") +
    geom_line(aes(date, predicted), color = "blue") +
    #geom_line(aes(date, difference)) +
    labs(title = comp_summary$Name.y, subtitle = "Boosted tree model", y = "Percent", x = "Date")
  
#}
  
summary(comp_summary$difference)
  
median_difference <- median(comp_summary$difference, na.rm = TRUE)

#Testing nearest neighbor model:

bt_model <- boost_tree(mode = "regression", trees = tune(), mtry = tune(), learn_rate = tune()) %>%
  set_engine("xgboost")
#tune mtry & learn_rate!!!

knn_model <- nearest_neighbor(mode = "regression", neighbors = tune()) %>%
  set_engine("kknn")

name_of_model <- bt_model

name_of_model <- knn_model

prepped_grid = bt_grid

prepped_grid = knn_grid

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
#SET UP GRID HERE
#USE GRID SEARCH (look at pinned Campuswire post)
#IF THEN/ELSE statements and check to see if it's the name or a model or save as a parameter object (which is complicated)
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

#BOOSTED TREE
#logistic_param <- 
#  tibble(
#    trees = best_model$trees
#  )

#NEAREST NEIGHBOR
#logistic_param <- 
#  tibble(
#    neighbors = best_model$neighbors
#  )

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

#print(comp_results)

#ggplot(comp_summary)+
#  geom_line(aes(date, difference)) #+
#  labs(title = comp_summary$Name.y)

#ggplot(comp_summary)+
#  geom_line(aes(date, price_change), color = "red") +
#  geom_line(aes(date, predicted), color = "blue") +
#geom_line(aes(date, difference)) +
#  labs(title = comp_summary$Name.y, subtitle = model_name, y = "Percent", x = "Date")

#make the differences absolute values, or square difference and root mean to do RMSE

avg_difference <- mean(abs(comp_summary$difference), na.rm = TRUE)

Issues:
  - Can't seem to fold data at all without issues

  - Nearest neighbor: "Error: Internal error: `check_installs()` should have caught an `unknown` mode."
  
  - Boosted tree: "Error: Internal error: `check_installs()` should have caught an `unknown` mode."
  
  - Linear regression: "Error in UseMethod("predict") : 
  no applicable method for 'predict' applied to an object of class "c('resample_results', 'tune_results', 'tbl_df', 'tbl', 'data.frame')""
  
- Can't seem to use other data model types without issues

- Nearest neighbor: "Error in data.frame(true_price_change$price_change, comp_predicted, (((true_price_change$price_change/comp_predicted) -  : 
  arguments imply differing number of rows: 22, 21" (One row has been deleted from the predicted data - it at least does predict the data though)

- Other errors:
  - Solution to this problem (removing lines with is.na) led to this error at the beginning: "object '*tmp*' not found"


Notes:
  -fold maybe four or five times
-make functions more efficient - hand off split data and folds!
  -finding each company's best model - "an ensamble model" and aggregating that data - trends in models for sectors (or maybe not)
-knn model!! elastic-net model!! (pick this for third) tune them!! (maybe? your data is small, focus on boosted tree boosting)

Take...
-formal regression class!!
-more python!
-social media and cultural analytics (and data for social good)
-IMC

use this for next quarter's project for data viz

More notes:
  - using yardstick to pick which model had the best metrics for each company
- chapter 10 - getting metrics out of resampled objects
- get metrics from original split data
- then bring in new data - just use the model created from the resampled data

Even more notes:
  
  - volume, high and low aren't knowable before the close happens
- use snapshots in time for the individual company data
- get the percent change in the dataset instead (14 day change)
- realign closing price with previous day information
- sketch out dataset: what should be here?
- narrow question - focus on percent change ACROSS models instead of focusing on ranking (unless for the final model)