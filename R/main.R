# Install the packages as it was last replicated + use here package ============
## Uncheck the following lines if the packages are not installed
# install.packages("renv")
# install.packages("here")
# renv::restore()

## If renv is giving you trouble due to OS issues or such, use the following:
# install.packages("remotes")
# library(remotes)
## Load remotes package
library(remotes)

## Install the required packages with specific versions 
## via remotes::install_version
# install_version("rstudioapi", version = "0.16.0")
# install_version("semver", version = "0.2.0")
# install_version("magrittr", version = "2.0.3")
# install_version("MCMCpack", version = "1.7-1")
# install_version("vctrs", version = "0.6.5")
# install_version("rstatix", version = "0.7.2")
# install_version("htmltools", version = "0.5.8.1")
# install_version("broom", version = "1.0.6")
# install_version("Formula", version = "1.2-5")
# install_version("parallelly", version = "1.38.0")
# install_version("htmlwidgets", version = "1.6.4")
# install_version("mime", version = "0.12")
# install_version("lifecycle", version = "1.0.4")
# install_version("iterators", version = "1.0.14")
# install_version("pkgconfig", version = "2.0.3")
# install_version("Matrix", version = "1.7-0")
# install_version("R6", version = "2.5.1")
# install_version("fastmap", version = "1.2.0")
# install_version("rbibutils", version = "2.2.16")
# install_version("shiny", version = "1.9.1")
# install_version("snakecase", version = "0.11.1")
# install_version("digest", version = "0.6.37")
# install_version("useful", version = "1.2.6.1")
# install_version("numDeriv", version = "2016.8-1.1")
# install_version("colorspace", version = "2.1-1")
# install_version("GGally", version = "2.2.1")
# install_version("wdman", version = "0.2.6")
# install_version("rprojroot", version = "2.0.4")
# install_version("fansi", version = "1.0.6")
# install_version("timechange", version = "0.3.0")
# install_version("httr", version = "1.4.7")
# install_version("abind", version = "1.4-5")
# install_version("compiler", version = "4.4.1")
# install_version("withr", version = "3.0.1")
# install_version("backports", version = "1.5.0")
# install_version("carData", version = "3.0-5")
# install_version("ggstats", version = "0.6.0")
# install_version("highr", version = "0.11")
# install_version("Rttf2pt1", version = "1.3.12")
# install_version("R.utils", version = "2.12.3")
# install_version("ggsignif", version = "0.6.4")
# install_version("RSelenium", version = "1.7.9")
# install_version("ggmcmc", version = "1.5.1.1")
# install_version("quantreg", version = "5.98")
# install_version("caTools", version = "1.18.3")
# install_version("label.switching", version = "1.8")
# install_version("tools", version = "4.4.1")
# install_version("extrafontdb", version = "1.0")
# install_version("httpuv", version = "1.6.15")
# install_version("R.oo", version = "1.26.0")
# install_version("glue", version = "1.7.0")
# install_version("rcdd", version = "1.6")
# install_version("nlme", version = "3.1-164")
# install_version("R.cache", version = "0.16.0")
# install_version("stringmagic", version = "1.1.2")
# install_version("promises", version = "1.3.0")
# install_version("grid", version = "4.4.1")
# install_version("reshape2", version = "1.4.4")
# install_version("generics", version = "0.1.3")
# install_version("lpSolve", version = "5.6.20")
# install_version("gtable", version = "0.3.5")
# install_version("labelled", version = "2.13.0")
# install_version("tzdb", version = "0.4.0")
# install_version("R.methodsS3", version = "1.8.2")
# install_version("hms", version = "1.1.3")
# install_version("car", version = "3.1-2")
# install_version("utf8", version = "1.2.4")
# install_version("foreach", version = "1.5.2")
# install_version("pillar", version = "1.9.0")
# install_version("later", version = "1.3.2")
# install_version("splines", version = "4.4.1")
# install_version("lattice", version = "0.22-6")
# install_version("renv", version = "1.0.7")
# install_version("survival", version = "3.6-4")
# install_version("dreamerr", version = "1.4.0")
# install_version("SparseM", version = "1.84-2")
# install_version("miniUI", version = "0.1.1.1")
# install_version("gridExtra", version = "2.3")
# install_version("fontcm", version = "1.1")
# install_version("mcmc", version = "0.9-8")
# install_version("statmod", version = "1.5.0")
# install_version("stringi", version = "1.8.4")
# install_version("codetools", version = "0.2-20")
# install_version("extrafont", version = "0.19")
# install_version("cli", version = "3.6.3")
# install_version("Rdpack", version = "2.6.1")
# install_version("munsell", version = "0.5.1")
# install_version("Rcpp", version = "1.0.13")
# install_version("globals", version = "0.16.3")
# install_version("coda", version = "0.19-4.1")
# install_version("binman", version = "0.1.3")
# install_version("parallel", version = "4.4.1")
# install_version("radarchart", version = "0.3.1")
# install_version("MatrixModels", version = "0.5-3")
# install_version("bitops", version = "1.0-8")
# install_version("listenv", version = "0.9.1")
# install_version("ggthemes", version = "5.1.0")
# install_version("rlang", version = "1.1.4")

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
## This script is a heavy load on the memory due to future_sapply
## Recommend at least 32gb of memory (max. ~59% used if all workers used)
## I've suppressed the number of workers to half the available ones
## If unreleased memory becomes an issue, try restarting session after this
## and run afresh from Step 07
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
