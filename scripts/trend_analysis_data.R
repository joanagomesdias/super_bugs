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
    ggplot2,
    broom)


# Importing the data -----------------------------------------------------

amr <- read_delim("data/dataset_amr.csv", 
                           ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                           trim_ws = TRUE)


# Linear regression ------------------------------------------------------
## Consumption -----
ggplot(amr) + geom_histogram(mapping = aes(x = consumo_ddd_hab_day)) + xlab("Consumo ddd hab day")

# Linear regression between consumption and time
lm.comsumption <- lm(consumo_ddd_hab_day ~ ano, data = amr)
summary(lm.comsumption)

# Summary in table form
tidy(lm.comsumption, conf.int = TRUE)

# Creates a new data frame with the original data and the model values added
lm.comsumption_dt <- as.data.frame(augment(lm.comsumption))

# Plot observed vs. fitted
ggplot(lm.comsumption_dt) + 
  geom_point(aes(x = ano, y = consumo_ddd_hab_day), alpha = 0.3) +
  geom_line(aes(x = ano, y = .fitted))


## Resistance -----
ggplot(amr) + geom_histogram(mapping = aes(x = resistencias_crkp_perc)) + xlab("Resistencias crkp (%)")

# Linear regression between resistance and time
lm.resistence <- lm(resistencias_crkp_perc ~ ano, data = amr)
summary(lm.resistence)

# Summary in table form
tidy(lm.resistence, conf.int = TRUE)

# Creates a new data frame with the original data and the model values added
lm.resistance_dt <- as.data.frame(augment(lm.resistence))

# Plot observed vs. fitted
ggplot(lm.resistance_dt) + 
  geom_point(aes(x = ano, y = resistencias_crkp_perc), alpha = 0.3) +
  geom_line(aes(x = ano, y = .fitted))


## Consumption vs. Resistance -----

# Linear regression between consumption and resistance
lm.resist.comsump <- lm(resistencias_crkp_perc ~ consumo_ddd_hab_day, data = amr)
summary(lm.resist.comsump)

# Summary in table form
tidy(lm.resist.comsump, conf.int = TRUE)

# Creates a new data frame with the original data and the model values added
lm.resist.comsump_dt <- as.data.frame(augment(lm.resist.comsump))

# Plot observed vs. fitted
ggplot(lm.resist.comsump_dt) + 
  geom_point(aes(x = consumo_ddd_hab_day, y = resistencias_crkp_perc), alpha = 0.3) +
  geom_line(aes(x = consumo_ddd_hab_day, y = .fitted))
 