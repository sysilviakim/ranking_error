# ranking_error

The code in this replication package constructes the files for Atsusaka and Kim (2024), Addressing Measurement Errors in Ranking Questions for the Social Sciences. For full paper, see <https://osf.io/preprints/osf/3ys8x>.

- This was last replicated with R version 4.4.1 (2024-06-14 ucrt), Platform: x86_64-w64-mingw32/x64, Windows 11 x64 (build 22631), 32.0 GB RAM, Intel(R) Core(TM) Ultra 7 155H, 3800 Mhz, 16 Core(s), 22 Logical Processor(s). The estimated computation time on a comparable machine is about 43 minutes.
- Running `R/main.R` allows for all replication of the figures/tables.
- The `data/raw` folder contains all the raw data used in the replication.
- The version of packages used are all recorded with the `renv` package, via `renv.lock` in the root directory. While `renv::restore()` should install all necessary packages, if for some reason `renv::restore()` fails, follow the specific installation guidelines in `main.R` using the `remotes` package.
