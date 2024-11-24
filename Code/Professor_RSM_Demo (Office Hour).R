
library(dplyr)
#install.packages("readr")
library(readr)
#install.packages("tidyr")
library(tidyr)
library(ggplot2)
#install.packages("viridis")
library(viridis)

data = tribble(
  ~speed, ~car, ~x1, ~x2,
  102,    "A",   3,   5,
  53,     "B",   4,   2,
  48,      "A",  3,   8,
  55,      "B",  2,   5,
  54,      "B",  2,   5,
  32,      "B",  2,   5,
  52,      "B",  2,   5,
  33,      "B",  2,   5,
  77,      "B",  2,   5
)


m1 = data %>% lm(formula = speed ~ x1 + x2 + car)

newdata = expand_grid(
  x1 = seq(from = 1, to = 10, by = 1),
  x2 = seq(from = 1, to = 10, by = 1)
) %>% 
  mutate(car = "A")


pred = tibble(
  newdata,
  yhat = predict(object = m1, newdata = newdata)
)

ggplot() +
  geom_tile(data = pred, mapping = aes(x = x1, y = x2, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma")


pred %>%
  filter(yhat == max(yhat))


# Interactions
m2 = data %>% lm(formula = speed ~ x1 + x2 + I(x1*x2) + car)
newdata = expand_grid(
  x1 = seq(from = 1, to = 10, by = 1),
  x2 = seq(from = 1, to = 10, by = 1)
) %>% 
  mutate(car = "A")


pred = tibble(
  newdata,
  yhat = predict(object = m2, newdata = newdata)
)

ggplot() +
  geom_tile(data = pred, mapping = aes(x = x1, y = x2, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma")



# Polynomial with Interaction
m3 = data %>% lm(formula = speed ~ poly(x1, 2) + poly(x2, 2) + I(x1*x2) + car)
broom::glance(m3)
newdata = expand_grid(
  x1 = seq(from = 1, to = 10, by = 1),
  x2 = seq(from = 1, to = 10, by = 1)
) %>% 
  mutate(car = "A")


pred = tibble(
  newdata,
  yhat = predict(object = m3, newdata = newdata)
)

ggplot() +
  geom_tile(data = pred, mapping = aes(x = x1, y = x2, fill = yhat)) +
  viridis::scale_fill_viridis(option = "plasma")
