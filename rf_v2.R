library(tidyverse)
library(vroom)
library(sf)
library(tidymodels)
################################################################################

################################################################################
# data load ----
################################################################################
full_shp <- read_sf("intermediates/full_data.geojson")

df <- 
  full_shp %>% 
  as_tibble() %>% 
  select(-geometry)

################################################################################
# data prep ---- 
################################################################################
#split, controlling for narginacion

set.seed(123)

first_split <- 
  df %>% 
  drop_na() %>% 
  mutate(ratio_fem = popfem/pop) %>% 
  select(-c(1:13), -c("delitos", "popfem", "POB_TOTAL")) %>% 
  initial_split(data = ., 
                strata = GM_2020)


inc_delic_train <- training(x = first_split)
inc_delic_testn <-  testing(x = first_split)


# model def ----

rf_recipe <- recipe(delitos_cien_k ~ ., data = inc_delic_train) 

tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tune_spec

################################################################################
# model tuning ----
################################################################################

#xfold 
set.seed(234)
train_xfold <- vfold_cv(inc_delic_train)

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)






set.seed(345)


tree_wf <- 
  workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(rf_recipe)

tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = train_xfold,
    grid = tree_grid
  )


tree_res %>% 
  collect_metrics()

tree_res %>%
  collect_metrics() %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

tree_res$.metrics

tree_res %>%
  show_best("rmse")

tree_res %>%
  show_best("rsq")

best_tree <- 
  tree_res %>%
  select_best("rmse")

final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

set.seed(345)
final_fit <- 
  final_wf %>%
  last_fit(first_split) 

rf_i <- final_fit %>% pluck(".workflow", 1) %>% extract_fit_parsnip()

importancias <-
  rf_i$fit$variable.importance %>% 
  data.frame(importancia = .) %>% 
  rownames_to_column(var = "variable") %>% 
  as_tibble() %>% 
  arrange((importancia)) %>% 
  mutate(variable = as_factor(variable))

importancias %>% 
  ggplot() + 
  aes(x = importancia, y = variable) + 
  geom_bar(stat="identity")
