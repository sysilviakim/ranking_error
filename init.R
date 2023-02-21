renv::init()

install.packages("devtools")
install.packages("remotes")
install.packages("colorspace")
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
Kmisc::proj_skeleton()

# Install typically used libraries
install.packages("plyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")
install.packages("assertthat")
install.packages("janitor")
install.packages("xtable")
install.packages("styler")

# For this project
install.packages("Matrix")
install.packages("combinat")
install.packages("gtools")
install.packages("PLMIX")

renv::snapshot()
## Sys.setenv(RENV_DOWNLOAD_METHOD = "libcurl")
