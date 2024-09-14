# Install the packages as it was last replicated + use here package ============
## Last replicated with
## R version 4.4.1 (2024-06-14 ucrt)
## Platform: x86_64-w64-mingw32/x64
## Running under: Windows 11 x64 (build 22631)
## 32.0 GB RAM, Intel(R) Core(TM) Ultra 7 155H, 3800 Mhz, 16 Core(s), 
## 22 Logical Processor(s)
## The estimated computation time on a comparable machine is about 43 minutes.

## Uncomment the following lines if the packages are not installed
# install.packages("renv")
# install.packages("here")
# renv::restore()

## If renv is giving you trouble due to OS issues or such, use the following:
# install.packages("remotes")
# library(remotes)
## Load remotes package
# Load remotes package
# library(remotes)
# install_version("corrplot", version = "0.94")
# install_version("coefplot", version = "1.2.8")
# install_version("fixest", version = "0.12.1")
# install_version("stargazer", version = "5.2.3")
# install_version("sandwich", version = "3.1-0")
# install_version("lmtest", version = "0.9-40")
# install_version("zoo", version = "1.8-12")
# install_version("future.apply", version = "1.11.2")
# install_version("future", version = "1.34.0")
# install_version("mlogit", version = "1.1-1")
# install_version("dfidx", version = "0.1-0")
# install_version("clarify", version = "0.2.1")
# install_version("RColorBrewer", version = "1.1-3")
# install_version("questionr", version = "0.7.8")
# install_version("haven", version = "2.5.4")
# install_version("ggpubr", version = "0.6.0")
# install_version("estimatr", version = "1.0.4")
# install_version("xtable", version = "1.8-4")
# install_version("pwr", version = "1.3-0")
# install_version("assertthat", version = "0.2.1")
# install_version("scales", version = "1.3.0")
# install_version("janitor", version = "2.2.0")
# install_version("patchwork", version = "1.2.0")
# install_version("PLMIX", version = "2.1.1")
# install_version("styler", version = "1.10.3")
# install_version("lubridate", version = "1.9.3")
# install_version("forcats", version = "1.0.0")
# install_version("stringr", version = "1.5.1")
# install_version("dplyr", version = "1.1.4")
# install_version("purrr", version = "1.0.2")
# install_version("readr", vesrsion = "2.1.5")
# install_version("tidyr", version = "1.3.1")
# install_version("tibble", version = "3.2.1")
# install_version("ggplot2", version = "3.5.1")
# install_version("tidyverse", version = "2.0.0")
# install_version("tidyselect", version = "1.2.1")
# install_version("gtools", version = "3.9.5")
# install_version("combinat", version = "0.0-8")
# install_version("here", version = "1.0.1")
# install_version("MASS", version = "7.3-60.2")
# install_version("plyr", version = "1.8.9")

## None-CRAN packages
# library(remotes)
# install_github(
#   "sysilviakim/Kmisc",
#   INSTALL_opts = c("--no-multiarch"),
#   dependencies = TRUE
# )
# install_github(
#   "sysilviakim/rankingQ",
#   INSTALL_opts = c("--no-multiarch"),
#   dependencies = TRUE
# )

# Figures and Tables replication, ordered ======================================
## Step 00: simulation and the main bias correction
suppressMessages(suppressWarnings(source(here::here("R", "FigB2.R"))))
message("FigB2 successfully completed.")

## Step 01
suppressMessages(suppressWarnings(source(here::here("R", "FigC11.R"))))
message("FigC11 successfully completed.")

## Step 02
suppressMessages(suppressWarnings(source(here::here("R", "FigB4.R"))))
message("FigB4 successfully completed.")

suppressMessages(suppressWarnings(source(here::here("R", "Fig6.R"))))
message("Fig6 successfully completed.")

suppressMessages(suppressWarnings(source(here::here("R", "FigsC12-C15.R"))))
message("Figs C12 to C15 successfully completed.")

## Step 03
## This one does not produce figures or tables
suppressMessages(suppressWarnings(source(here::here("R", "bias_correction.R"))))
message("Main bias correction script completed.")

## Step 04
suppressMessages(suppressWarnings(source(here::here("R", "Fig8.R"))))
message("Fig8 successfully completed.")

## Step 05
suppressMessages(suppressWarnings(source(here::here("R", "Fig7.R"))))
message("Fig7 successfully completed.")

## Step 06
## This script is a heavy load on the memory due to future_sapply
## Recommend at least 32gb of memory (max. ~59% used if all workers used)
## I've suppressed the number of workers to half the available ones
## If unreleased memory becomes an issue, try restarting session after this
## and run afresh from Step 07
suppressMessages(suppressWarnings(source(here::here("R", "Fig9.R"))))
message("Fig9 successfully completed.")

## Step 07
suppressMessages(suppressWarnings(source(here::here("R", "FigC16.R"))))
message("FigC16 successfully completed.")

## Step 08
suppressMessages(suppressWarnings(source(here::here("R", "Fig5.R"))))
message("Fig5 successfully completed.")

## Step 09
suppressMessages(suppressWarnings(source(here::here("R", "TabC1.R"))))
message("TabC1 successfully completed.")

suppressMessages(suppressWarnings(source(here::here("R", "FigC10.R"))))
message("FigC10 successfully completed.")

suppressMessages(suppressWarnings(source(here::here("R", "Tab2.R"))))
message("Tab2 successfully completed.")

## Step 10
suppressMessages(suppressWarnings(source(here::here("R", "Fig10.R"))))
message("Fig10 successfully completed.")
message("All scripts successfully completed.")
