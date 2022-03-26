# Script information ------------------------------------------------------

#' Project title: AMR
#' Script name: 0x_trend_import.R
#' Date created: 2022-03-21
#' Date updated: 2022-MM-DD
#' Author: Joana Gomes Dias, Bruno Macedo, Gean Sa and Marvin OLiveira
#' Script purpose: This script aims to import the datasets for this project


# Load packages ----------------------------------------------------------
# Ensures the package "pacman" is installed
if (!require("pacman")) install.packages("pacman")

# # Packages available from CRAN
 pacman::p_load(
    readr,
    dplyr,
    ggplot2,
    broom)


# Importing the data -----------------------------------------------------

amr <- read_delim("data/dataset_amr.csv", 
                           ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                           trim_ws = TRUE)

amr2014_2020 <- amr %>% 
  filter(ano <= 2020)

amr.pred2021_2030 <- amr %>% 
  select(ano) %>% 
  filter(ano >= 2021)

# Linear model regression ------------------------------------------------------
## Consumption -----
ggplot(amr2014_2020) + 
   geom_histogram(mapping = aes(x = consumo_ddd_hab_day)) + 
   xlab("Consumo ddd hab day")
# Linear regression between consumption and time
lm.consumption <- lm(consumo_ddd_hab_day ~ ano, data = amr2014_2020)
# Summary in table form
tidy(lm.consumption, conf.int = TRUE)
# Creates a new data frame with the original data and the model values added
lm.consumption_dt <- as.data.frame(augment(lm.consumption))
lm.consumption_dt <- lm.consumption_dt %>% 
  select(consumo_ddd_hab_day, ano, .fitted)
# Plot observed vs. fitted
ggplot(lm.consumption_dt) + 
  geom_point(aes(x = ano, y = consumo_ddd_hab_day), alpha = 0.3) +
  geom_line(aes(x = ano, y = .fitted))
# Predictions consumption
pred.lm.consumption <- predict(lm.consumption, newdata = amr.pred2021_2030, interval = "confidence")
# Data preparation final data frame
# Add year
amr.pred2021_2030_consumption <- cbind(amr.pred2021_2030, pred.lm.consumption)
# Rename fit
names(amr.pred2021_2030_consumption)[names(amr.pred2021_2030_consumption) == 'fit'] <- '.fitted'
# Join dataframe will all data
consumption <- full_join(lm.consumption_dt, amr.pred2021_2030_consumption, by = c("ano", ".fitted"))

## Resistance -----
ggplot(amr2014_2020) + 
  geom_histogram(mapping = aes(x = resistencias_crkp_perc)) + 
  xlab("Resistencias crkp (%)")
# Linear regression between resistance and time
lm.resistance <- lm(resistencias_crkp_perc ~ ano, data = amr2014_2020)
# Summary in table form
tidy(lm.resistance, conf.int = TRUE)
# Creates a new data frame with the original data and the model values added
lm.resistance_dt <- as.data.frame(augment(lm.resistance))
# Plot observed vs. fitted
ggplot(lm.resistance_dt) + 
  geom_point(aes(x = ano, y = resistencias_crkp_perc), alpha = 0.3) +
  geom_line(aes(x = ano, y = .fitted))
# Predictions resistance
pred.lm.resistance <- predict(lm.resistance, newdata = amr.pred2021_2030, interval = "confidence")
# Data preparation final data frame
# Add year
amr.pred2021_2030_resistance <- cbind(amr.pred2021_2030, pred.lm.resistance)
# Rename fit
names(amr.pred2021_2030_resistance)[names(amr.pred2021_2030_resistance) == 'fit'] <- '.fitted'
# Join dataframe will all data
resistance <- full_join(lm.resistance_dt, amr.pred2021_2030_resistance, by = c("ano", ".fitted"))


## Consumption vs. Resistance -----
# Linear regression between consumption and resistance
lm.resist.comsump <- lm(resistencias_crkp_perc ~ consumo_ddd_hab_day, data = amr2014_2020)
# Summary in table form
tidy(lm.resist.comsump, conf.int = TRUE)
# Creates a new data frame with the original data and the model values added
lm.resist.comsump_dt <- as.data.frame(augment(lm.resist.comsump))
# Plot observed vs. fitted
ggplot(lm.resist.comsump_dt) + 
  geom_point(aes(x = consumo_ddd_hab_day, y = resistencias_crkp_perc), alpha = 0.3) +
  geom_line(aes(x = consumo_ddd_hab_day, y = .fitted))
# Predictions consumption vs. resistance
# to be discussed


# Log-linear model regression --------------------------------------------------
## Consumption -----
# Log-Linear regression between consumption and time
lm.consumption.log <- lm(log(consumo_ddd_hab_day) ~ ano, data = amr2014_2020)
# Summary in table form
tidy(lm.consumption.log, exponentiate = TRUE, conf.int = TRUE)
# Creates a new data frame with the original data and the model values added
lm.consumption.log_dt <- as.data.frame(augment(lm.consumption.log))
names(lm.consumption.log_dt)[names(lm.consumption.log_dt) == 'log(consumo_ddd_hab_day)'] <- 'log_consump'
# Plot observed vs. fitted
ggplot(lm.consumption.log_dt) + 
  geom_point(aes(x = ano, y = exp(log_consump), alpha = 0.3) )+
  geom_line(aes(x = ano, y = exp(.fitted)))
# Predictions consumption
pred.lm.consumption.log <- predict(lm.consumption.log, newdata = amr.pred2021_2030, interval = "confidence")
# Data preparation final data frame
# Add year
amr.pred2021_2030_consumption.log <- cbind(amr.pred2021_2030, pred.lm.consumption.log)
# Rename fit
names(amr.pred2021_2030_consumption.log)[names(amr.pred2021_2030_consumption.log) == 'fit'] <- '.fitted'
# Join dataframe will all data
consumption.log <- full_join(lm.consumption.log_dt, amr.pred2021_2030_consumption.log, by = c("ano", ".fitted"))


## Resistance -----
# Log-Linear regression between resistance and time
lm.resistance.log <- lm(log(resistencias_crkp_perc) ~ ano, data = amr2014_2020)
# Summary in table form
tidy(lm.resistance.log, exponentiate = TRUE, conf.int = TRUE)
# Creates a new data frame with the original data and the model values added
lm.resistance.log_dt <- as.data.frame(augment(lm.resistance.log))
names(lm.resistance.log_dt)[names(lm.resistance.log_dt) == 'log(resistencias_crkp_perc)'] <- 'log_resist'
# Plot observed vs. fitted
ggplot(lm.resistance.log_dt) + 
  geom_point(aes(x = ano, y = exp(log_resist), alpha = 0.3) )+
  geom_line(aes(x = ano, y = exp(.fitted)))
# Predictions resistance
pred.lm.resistance.log <- predict(lm.resistance.log, newdata = amr.pred2021_2030, interval = "confidence")
# Data preparation final data frame
# Add year
amr.pred2021_2030_resistance.log <- cbind(amr.pred2021_2030, pred.lm.resistance.log)
# Rename fit
names(amr.pred2021_2030_resistance.log)[names(amr.pred2021_2030_resistance.log) == 'fit'] <- '.fitted'
# Join dataframe will all data
resistance.log <- full_join(lm.resistance.log_dt, amr.pred2021_2030_resistance.log, by = c("ano", ".fitted"))



## Consumption vs. Resistance -----
# Log-log regression between consumption and resistance
lm.resist.comsump.log <- lm(log(resistencias_crkp_perc) ~ log(consumo_ddd_hab_day), data = amr2014_2020)
# Summary in table form
tidy(lm.resist.comsump.log, exponentiate = TRUE, conf.int = TRUE)
# Creates a new data frame with the original data and the model values added
lm.resist.comsump.log_dt <- as.data.frame(augment(lm.resist.comsump.log))
names(lm.resist.comsump.log_dt)[names(lm.resist.comsump.log_dt) == 'log(resistencias_crkp_perc)'] <- 'log_resist'
names(lm.resist.comsump.log_dt)[names(lm.resist.comsump.log_dt) == 'log(consumo_ddd_hab_day)'] <- 'log_consump'
# Plot observed vs. fitted
ggplot(lm.resist.comsump.log_dt) + 
  geom_point(aes(x = log_consump, y = log_resist), alpha = 0.3) +
  geom_line(aes(x = log_consump, y = .fitted))


