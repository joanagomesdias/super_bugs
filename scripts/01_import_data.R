# Script information ------------------------------------------------------

#' Project title: AMR
#' Script name: 1_data_import.R
#' Date created: 2022-03-21
#' Date updated: 2022-MM-DD
#' Author: Joana Gomes Dias, Bruno Macedo, Gean Sa and Marvin OLiveira
#' Script purpose: This script aims to import the datasets for this project


# Load packages ----------------------------------------------------------
# Ensures the package "pacman" is installed
if (!require("pacman")) install.packages("pacman")

# # Packages available from CRAN
# pacman::p_load(
#   dplyr               # data management
# 
# )



# Importing the data -----------------------------------------------------

resistence <- read.csv("data/xxx.csv")
comsumption <- read.csv("data/xxx.csv")
burden <- read.csv("data/xxx.csv")


# Checking dimensions -----------------------------------------------------

dim(resistence)
dim(comsumption)
dim(burden)

# Checking cells content --------------------------------------------------

str(resistence)
head(resistence)

str(comsumption)
head(comsumption)

str(burden)
head(burden)

# Exploring datasets -----------------------------------------------------





