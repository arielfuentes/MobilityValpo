library(tidymodels)

set.seed(123)
trips_split <- 
  initial_split(filter(trips, TipoServicio == "Urbano" & TipoRec != 0) %>% 
                  select(-c("Fecha", "Recaudación", "TipoServicio")) %>%
                  na.omit(), 
                strata = TipoRec, prop = .75)
trips_train <- training(trips_split)
trips_test <- testing(trips_split)
################################################
#define model ----
rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>%
  set_mode("regression")
#set recipe ----
rf_recipe <- recipe(Demanda ~ ., data = trips_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_log(tviaje, offset = 1) %>%
  step_normalize(all_predictors()) 
  
#create workflow ----
rf_wkf <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)
#fit model ----
set.seed(234)
rf_fit <- rf_wkf %>%
  fit(trips_train)
#prediction ----
rf_train_pred <- predict(rf_fit, trips_train) %>%
  bind_cols(select(trips_train, -c("Tarifa", "tviaje")))

group_by(rf_train_pred, Servicio, TipoDia, HoraInicio) %>%
  summarise(Demanda = sum(Demanda), .pred = sum(.pred)) %>%
ggplot(aes(Demanda-.pred)) +
  geom_density(alpha = .7, fill = "lightblue") +
  facet_wrap(~TipoDia)

rf_all_pred <- predict(rf_fit, filter(trips, TipoServicio == "Urbano" & TipoRec != 0) %>% 
                         select(-c("Fecha", "Recaudación", "TipoServicio")) %>%
                         na.omit()) %>%
  bind_cols(filter(trips, TipoServicio == "Urbano" & TipoRec != 0) %>% 
              select(-c("Fecha", "Recaudación", "TipoServicio")) %>%
              na.omit())

group_by(rf_all_pred, Servicio, TipoDia, HoraInicio) %>%
  summarise(Demanda = sum(Demanda), .pred = sum(.pred)) %>%
  ggplot(aes(Demanda-.pred)) +
  geom_density(alpha = .7, fill = "lightblue") +
  facet_wrap(~TipoDia)
##############################################
trips_recipe <- 
  recipe(Demanda ~ ., data = trips_train) %>%
  step_dummy(all_nominal(), -all_outcomes())

trips_prep <- prep(trips_recipe)
juiced <- juice(trips_prep)


rf_model <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

rf_wf <- workflow() %>%
  # add the recipe
  add_recipe(trips_recipe) %>%
  # add the model
  add_model(rf_model)

set.seed(234)
trips_cv <- vfold_cv(trips_train)

doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  rf_wf,
  resamples = trips_cv,
  grid = 10
)

tune_res

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rsq")

rf_grid <- grid_regular(
  mtry(range = c(5, 16)),
  min_n(range = c(0, 30)),
  levels = 5
)

rf_grid

set.seed(456)
regular_res <- tune_grid(
  rf_wf,
  resamples = trips_cv,
  grid = rf_grid
)

regular_res

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rsq")

best_rsq <- select_best(regular_res, "rsq")

final_rf <- finalize_model(
  rf_model,
  best_rsq
)

final_rf

library(vip)

rf_model %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Demanda ~ .,
      data = juiced) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(trips_recipe) %>%
  add_model(rf_model)

final_res <- final_wf %>%
  last_fit(trips_split)

final_res %>%
  collect_metrics()

final_res %>%
  collect_predictions() %>%
  ggplot(aes(Pax, .pred)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)

final_model <- 
  fit(final_wf, mvl3 %>% 
        select(-c("Pasajero", "Recaudación", "Sentido")))

predict(final_model, new_data = Elec_new %>% 
          select(-c("Pasajero", "Recaudación", "Sentido"))) %>%
  bind_cols(Elec_new) %>%
  group_by(Servicio, Día) %>%
  summarise(PaxPred = sum(.pred))

predict(final_model, new_data = filter(mvl3, !Servicio %in% c("E01", "E02")) %>% 
          select(-c("Pasajero", "Recaudación", "Sentido"))) %>%
  bind_cols(filter(mvl3, !Servicio %in% c("E01", "E02"))) %>%
  ggplot(aes(.pred-Pax)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)
# group_by(Servicio, Día) %>%
#   summarise(PaxPred = sum(.pred),
#             Pax = sum(Pax))
ranger_obj <- pull_workflow_fit(final_model)$fit