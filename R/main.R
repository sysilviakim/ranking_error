# Install the packages as it was last replicated + use here package ============
install.packages("renv")
install.packages("here")
renv::restore()
library(here)

## If renv is giving you trouble due to OS issues or such, use the following:
install.packages("remotes")
library(remotes)
## install_version("")

# Figures and Tables replication, ordered ======================================
## Step 00: simulation and the main bias correction
source(here::here("R", "FigB2.R"))

## Step 01
source(here::here("R", "FigC11.R"))

## Step 02
source(here::here("R", "FigB4.R"))
source(here::here("R", "Fig6.R"))
source(here::here("R", "FigsC12-C15.R"))

## Step 03
## This one does not produce figures or tables
source(here::here("R", "bias_correction.R"))

## Step 04
source(here::here("R", "Fig8.R"))

## Step 05
source(here::here("R", "Fig7.R"))

## Step 06
source(here::here("R", "Fig9.R"))

## Step 07
source(here::here("R", "FigC16.R"))

## Step 08
source(here::here("R", "Fig5.R"))

## Step 09
source(here::here("R", "TabC1.R"))
source(here::here("R", "FigC10.R"))
source(here::here("R", "Tab2.R"))

## Step 10
source(here::here("R", "Fig10.R"))
