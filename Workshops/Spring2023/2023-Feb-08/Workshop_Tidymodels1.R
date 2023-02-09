# Title: ML in R Workshop 1
# Author: Greg Chism
# Date: 2023-02-08
# Description: Intro to Tidymodels in R

# Installing required packages
install.packages("pacman")

library(pacman)

p_load(broom.mixed,
       dotwhisker,
       RCurl,
       rstanarm,
       tidymodels,
       tidyverse)

# Read in and examine data
data <- getURL("https://raw.githubusercontent.com/Gchism94/Data7_EDA_In_R_Workshops/main/Data7_EDA_In_R_Book/data/Data_Fig2_Repo.csv")
data <- read.csv(text = data)

data %>% head()

# Plot out data before modeling
data %>%
  ggplot(aes(x = pLWP, y = mLWP, group = Group, color = Group)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_minimal(base_size = 14)

# Model formula
# mLWP ~ pLWP * Group

linear_reg()

linear_reg() %>%
  set_engine("keras")

lm_mod <- linear_reg()

lm_fit <- 
  lm_mod %>%
  fit(mLWP ~ pLWP * Group, data = data)

lm_fit

tidy(lm_fit)

# Visualize our regression with dotwisker

tidy(lm_fit) %>%
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2)) +
  theme_minimal(base_size = 14)

# Predict values with our model
new_points <- expand.grid(pLWP = -1.75, 
                          Group = c("Drought-sens-canopy",
                                    "Drought-sens-under",
                                    "Drought-tol-canopy",
                                    "Drought-tol-under"))

new_points

mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred

conf_int_pred <- predict(lm_fit,
                         new_data = new_points, 
                         type = "conf_int")
conf_int_pred

# Plot our predicted values
plot_data <-
  new_points %>%
  bind_cols(mean_pred) %>%
  bind_cols(conf_int_pred)

plot_data

ggplot(plot_data, aes(x = Group)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper),
                width = 0.2) +
  labs(y = "mLWP") +
  theme_minimal(base_size = 14)


# Model with a different engine 
prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

# make our model
bayes_mod <- 
  linear_reg() %>%
  set_engine("stan",
             prior_intercept = prior_dist,
             prior = prior_dist)


bayes_mod

# train our new model
bayes_fit <- 
  bayes_mod %>%
  fit(mLWP ~ pLWP * Group, data = data)

print(bayes_fit, digits = 3)

tidy(bayes_fit, conf.int = TRUE)

# visualize our bayesian model
bayes_plot_data <- 
  new_points %>%
  bind_cols(predict(bayes_fit, new_data = new_points)) %>%
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))
bayes_plot_data  

ggplot(bayes_plot_data, aes(x = Group)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper),
                width = 0.2) +
  labs(y = "mLWP") +
  theme_minimal(base_size = 14) +
  ggtitle("Bayesian model with t(1) prior distribution")

