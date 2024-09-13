renv::init()

utils::install.packages("devtools")
utils::install.packages("remotes")
utils::install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
install_github(
  "wch/extrafont", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
install_github(
  "wch/fontcm", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
install_github(
  "sysilviakim/rankingQ",
  INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
Kmisc::proj_skeleton()

# Install typically used libraries
utils::install.packages("plyr")
utils::install.packages("tidyverse")
utils::install.packages("lubridate")
utils::install.packages("here")
utils::install.packages("assertthat")
utils::install.packages("janitor")
utils::install.packages("xtable")
utils::install.packages("styler")

# For this project
utils::install.packages("Matrix")
utils::install.packages("combinat")
utils::install.packages("gtools")
utils::install.packages("PLMIX")

renv::snapshot()
## Sys.setenv(RENV_DOWNLOAD_METHOD = "libcurl")
