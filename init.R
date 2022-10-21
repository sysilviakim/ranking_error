renv::init()

install.packages("devtools")
install.packages("remotes")
install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
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
