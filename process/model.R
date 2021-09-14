#libraries ----
library(tidymodels)
#split data ----
set.seed(123)
demand_split <- initial_split(data_f, 
                                  prop = 3/4, 
                                  strata = Tarifa)
demand_train <- training(demand_split)
demand_test <- testing(demand_split)
##training proportion ----
demand_train %>%
  count(Tarifa) %>%
  mutate(prop = n/sum(n))
demand_test %>%
  count(Tarifa) %>%
  mutate(prop = n/sum(n))
#define model ----
rf_mod <- 
  rand_forest(trees = 3000) %>% 
  set_engine("ranger") %>%
  set_mode("regression")
#set recipe ----
rf_recipe <- recipe(Demanda ~ ., data = demand_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())
#create workflow ----
rf_wkf <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)
#fit model ----
set.seed(234)
rf_fit <- rf_wkf %>%
  fit(demand_train)
#prediction ----
rf_train_pred <- predict(rf_fit, demand_train) %>%
  bind_cols(demand_train)

rf_test_pred <- predict(rf_fit, demand_test) %>%
  bind_cols(demand_test)
##pred plot ----
select(rf_train_pred, Demanda, .pred) %>%
  ggplot(aes(.pred, Demanda)) +
  geom_point() + 
  geom_smooth(formula= y~x)
select(rf_test_pred, Demanda, .pred) %>%
  ggplot(aes(.pred, Demanda)) +
  geom_point() + 
  geom_smooth(formula= y~x)
select(rf_train_pred, Demanda, .pred, Pasajero) %>%
  ggplot(aes(.pred, Demanda)) +
  geom_point() + 
  facet_wrap(~Pasajero) +
  geom_smooth(formula= y~x)
select(rf_test_pred, Demanda, .pred, Pasajero) %>%
  ggplot(aes(.pred, Demanda)) +
  geom_point() + 
  facet_wrap(~Pasajero) +
  geom_smooth(formula= y~x)

##test performance ----
rf_train_pred %>%
  metrics(Demanda, .pred)
rf_test_pred %>%
  metrics(Demanda, .pred)
##residual data
SUBEN_res <- rf_test_pred %>%
  arrange(.pred) %>%
  mutate(res = (Demanda - .pred)/.pred) %>%
  select(.pred, res)
###plot ----
ggplot(SUBEN_res, aes(.pred, res)) +
  geom_point() + 
  geom_smooth(formula= y~x)
##pred new data ----
rf_new_pred <- predict(rf_fit, new_data_pred) %>%
  bind_cols(data_f)
ggplot(rf_new_pred, aes(.pred, Demanda)) +
  geom_point() + 
  facet_wrap(~Pasajero) +
  geom_smooth(formula= y~x)
