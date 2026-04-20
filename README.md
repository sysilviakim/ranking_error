# ranking_error

This replication package constructs the files for Atsusaka, Yuki, and Seo-young Silvia Kim, "Addressing Measurement Errors in Ranking Questions for the Social Sciences," *Political Analysis* 33, no. 4 (2025): 339-60. <https://doi.org/10.1017/pan.2024.33>.

- This was last replicated with R version 4.4.1 (2024-06-14 ucrt), Platform: x86_64-w64-mingw32/x64, Windows 11 x64 (build 22631), 32.0 GB RAM, Intel(R) Core(TM) Ultra 7 155H, 3800 Mhz, 16 Core(s), 22 Logical Processor(s). The estimated computation time on a comparable machine is about 43 minutes.
- Running `R/main.R` allows for all replication of the figures/tables.
- The [`rankingQ`](https://github.com/sysilviakim/rankingQ) package has since been substantially patched, and the current version will not work with the replication code here. An exact replication requires using an older SHA of `rankingQ`.
- The `data/raw` folder contains all the raw data used in the replication.
- The version of packages used are all recorded with the `renv` package, via `renv.lock` in the root directory. While `renv::restore()` should install all necessary packages, if for some reason `renv::restore()` fails, follow the specific installation guidelines in `main.R` using the `remotes` package.

For an exact replication, install the older `rankingQ` version with:

```r
library(remotes)
install_github(
  "sysilviakim/rankingQ@b42cce2",
  INSTALL_opts = c("--no-multiarch"),
  dependencies = TRUE
)
```
