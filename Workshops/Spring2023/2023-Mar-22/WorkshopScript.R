# Title: Tidymodels - preprocessing data with recipes
# Author: Greg Chism
# Date: 2023-03-22
# Description: Preprocessing COVID-19 testing data for a logistic regression model

# Installing required packages
install.packages("pacman")

pacman::p_load(tidymodels,
               dlookr,
               RCurl)

# Set global themes for ggplot
theme_set(theme_minimal(base_size = 14))
theme_update(legend.position = "right")

# Read in the data
data <- getURL("https://raw.githubusercontent.com/Gchism94/Data7_EDA_In_R_Workshops/main/Data7_EDA_In_R_Book/data/daily_summary.csv")
data <- read.csv(text = data)

data %>% 
  head()

# Summary statistics
data %>%
  count(test_result) %>%
  mutate(prop = n/sum(n))

# Remove unneeded category (inconclusive)
data_mod <- 
  data %>%
  filter(test_result != "Inconclusive") %>%
  mutate(test_result = factor(test_result),
         date = lubridate::as_date(result_date)) %>%
  select(-result_date)

glimpse(data_mod)

# Data splitting
set.seed(222)

data_split <- initial_split(data_mod, prop = 3/4)

train_data <- training(data_split)

test_data <- testing(data_split)


# Creating recipes 
covid_rec <- 
  recipe(test_result ~ ., data = train_data) %>%
  update_role(test_source, date, new_role = "ID")

summary(covid_rec)

# Create features
covid_rec <- 
  recipe(test_result ~ ., data = train_data) %>%
  update_role(test_source, date, new_role = "ID") %>%
  step_date(date, features = c("dow", "month")) %>%
  step_holiday(date,
               holidays = timeDate::listHolidays("US"),
               keep_original_cols = FALSE) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) 

covid_rec

# Create a model with a recipe
lr_mod <- 
  logistic_reg() %>%
  set_engine("glm")

lr_mod

# Workflow for the model
covid_wflow <- 
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(covid_rec)

covid_wflow

# Fit model to data 
covid_fit <- 
  covid_wflow %>%
  fit(data = train_data)

covid_fit %>%
  extract_fit_parsnip() %>%
  tidy()

# Predicting classes
predict(covid_fit, test_data)

covid_aug <- 
  augment(covid_fit, test_data)

covid_aug %>%
  select(test_count, date, test_source, .pred_class, .pred_Positive)

# ROC curve 
covid_aug %>%
  roc_curve(truth = test_result, .pred_Positive) %>%
  autoplot()

covid_aug %>%
  roc_auc(truth = test_result, .pred_Negative)
  


