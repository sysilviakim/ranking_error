source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-03.csv")
main <- df_list$main

# Geometric patterning rates ===================================================
main %>%
  select(matches("^ns_")) %>%
  map( ~ sum(.x == 1) / nrow(main))

## Tate 1993: 48.0%
## Electoral systems: 84.7%
## Identity: 72.4%
## Polarization: 46.0%

# Correlation between attention filters ========================================
## Between two attention filters? ----------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "berinsky_fail")
# [1] 0.4462482
# Cond. on berinsky_fail == 1, Pr(ternovsky_fail == 1) is 63.3%
# Cond. on ternovsky_fail == 1, Pr(berinsky_fail == 1) is 76.0%

# Correlation between attention filters and geometric ==========================
### Tate 2003 ------------------------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "ns_tate")
# [1] 0.2347556
# Cond. on ns_tate == 1, Pr(ternovsky_fail == 1) is 36.2%
# Cond. on ternovsky_fail == 1, Pr(ns_tate == 1) is 68.0%

cor_and_condprob(main, "berinsky_fail", "ns_tate")
# [1] 0.47031
# Cond. on ns_tate == 1, Pr(berinsky_fail == 1) is 53.2%
# Cond. on berinsky_fail == 1, Pr(ns_tate == 1) is 83.3%

### Electoral systems ----------------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "ns_esystem")
# [1] 0.341133
# Cond. on ns_esystem == 1, Pr(ternovsky_fail == 1) is 25.5%
# Cond. on ternovsky_fail == 1, Pr(ns_esystem == 1) is 100.0%

cor_and_condprob(main, "berinsky_fail", "ns_esystem")
# [1] 0.5389262
# Cond. on ns_esystem == 1, Pr(berinsky_fail == 1) is 30.6%
# Cond. on berinsky_fail == 1, Pr(ns_esystem == 1) is 100.0%

### Identity -------------------------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "ns_identity")
# [1] 0.3084845
# Cond. on ns_identity == 1, Pr(ternovsky_fail == 1) is 33.8%
# Cond. on ternovsky_fail == 1, Pr(ns_identity == 1) is 96.0%

cor_and_condprob(main, "berinsky_fail", "ns_identity")
# [1] 0.2609297
# Cond. on ns_identity == 1, Pr(berinsky_fail == 1) is 58.6%
# Cond. on berinsky_fail == 1, Pr(ns_identity == 1) is 97.1%

### Voting mode ----------------------------------------------------------------
# cor_and_condprob(main, "ternovsky_fail", "ns_voting")
# cor_and_condprob(main, "berinsky_fail", "ns_voting")

## Between nonsincerity checks? ------------------------------------------------
## Tate and Nelson (relatively) don't align well, but this seems to be a Tate
## thing. Identity is more predictive, conditionally.

### Tate 2003 x Nelson 1997 ----------------------------------------------------
cor_and_condprob(main, "ns_tate", "ns_nelson")
# [1] 0.3194393
# Cond. on ns_nelson == 1, Pr(ns_tate == 1) is 63.3%
# Cond. on ns_tate == 1, Pr(ns_nelson == 1) is 66%

### Tate 2003 x Identity -------------------------------------------------------
cor_and_condprob(main, "ns_tate", "ns_identity")
# [1] 0.4765874
# Cond. on ns_identity == 1, Pr(ns_tate == 1) is 67.2%
# Cond. on ns_tate == 1, Pr(ns_identity == 1) is 83%

### Tate 2003 x Voting mode ----------------------------------------------------
# cor_and_condprob(main, "ns_tate", "ns_voting")

### Nelson 1997 x Identity -----------------------------------------------------
cor_and_condprob(main, "ns_nelson", "ns_identity")
# [1] 0.5503995
# Cond. on ns_identity == 1, Pr(ns_nelson == 1) is 72.4%
# Cond. on ns_nelson == 1, Pr(ns_identity == 1) is 85.7%

### Nelson 1997 x Voting mode --------------------------------------------------
cor_and_condprob(main, "ns_nelson", "ns_voting")

### Identity x Voting mode -----------------------------------------------------
cor_and_condprob(main, "ns_identity", "ns_voting")

# Partial rankers ==============================================================
## Who are the partial rankers?
## Can inattention predict partial ranking?

## Attention filter x partial ranking? -----------------------------------------
### Tate 1993 main question ----------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "partial_tate_main")
# [1] 0.4665595
# Cond. on partial_tate_main == 1, Pr(ternovsky_fail == 1) is 85.7%
# Cond. on ternovsky_fail == 1, Pr(partial_tate_main == 1) is 31.6%

cor_and_condprob(main, "berinsky_fail", "partial_tate_main")
# [1] 0.3738783
# Cond. on partial_tate_main == 1, Pr(berinsky_fail == 1) is 100%
# Cond. on berinsky_fail == 1, Pr(partial_tate_main == 1) is 20%

### Tate 1993 anchor question --------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "partial_tate_anc")
# [1] 0.5148993
# Cond. on partial_tate_anc == 1, Pr(ternovsky_fail == 1) is 87.5%
# Cond. on ternovsky_fail == 1, Pr(partial_tate_anc == 1) is 36.8%

cor_and_condprob(main, "berinsky_fail", "partial_tate_anc")
# [1] 0.3245785
# Cond. on partial_tate_anc == 1, Pr(berinsky_fail == 1) is 87.5%
# Cond. on berinsky_fail == 1, Pr(partial_tate_anc == 1) is 20%

### Nelson 1997 main question --------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "partial_nelson_main")
# [1] 0.3188285
# Cond. on partial_nelson_main == 1, Pr(ternovsky_fail == 1) is 50%
# Cond. on ternovsky_fail == 1, Pr(partial_nelson_main == 1) is 36.8%

cor_and_condprob(main, "berinsky_fail", "partial_nelson_main")
# [1] 0.2477307
# Cond. on partial_nelson_main == 1, Pr(berinsky_fail == 1) is 64.3%
# Cond. on berinsky_fail == 1, Pr(partial_nelson_main == 1) is 25.7%

### Nelson 1997 anchor question ------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "partial_nelson_anc")
# [1] 0.3185416
# Cond. on partial_nelson_anc == 1, Pr(ternovsky_fail == 1) is 54.5%
# Cond. on ternovsky_fail == 1, Pr(partial_nelson_anc == 1) is 31.6

cor_and_condprob(main, "berinsky_fail", "partial_nelson_anc")
# [1] 0.2780776
# Cond. on partial_nelson_anc == 1, Pr(berinsky_fail == 1) is 72.7%
# Cond. on berinsky_fail == 1, Pr(partial_nelson_anc == 1) is 22.9%

### Identity main question -----------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "partial_identity_main")
# [1] 0.3135416
# Cond. on partial_identity_main == 1, Pr(ternovsky_fail == 1) is 42.9%
# Cond. on ternovsky_fail == 1, Pr(partial_identity_main == 1) is 47.4%

cor_and_condprob(main, "berinsky_fail", "partial_identity_main")
# [1] 0.3423007
# Cond. on partial_identity_main == 1, Pr(berinsky_fail == 1) is 66.7%
# Cond. on berinsky_fail == 1, Pr(partial_identity_main == 1) is 40%

### Identity anchor question ---------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "partial_identity_anc")
# [1] 0.4594157
# Cond. on partial_identity_anc == 1, Pr(ternovsky_fail == 1) is 58.8%
# Cond. on ternovsky_fail == 1, Pr(partial_identity_anc == 1) is 52.6%

cor_and_condprob(main, "berinsky_fail", "partial_identity_anc")
# [1] 0.3934911
# Cond. on partial_identity_anc == 1, Pr(berinsky_fail == 1) is 76.5%
# Cond. on berinsky_fail == 1, Pr(partial_identity_anc == 1) is 37.1%

### Voting mode main question --------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "partial_voting_main")
# [1] 0.2852502
# Cond. on partial_voting_main == 1, Pr(ternovsky_fail == 1) is 42.1%
# Cond. on ternovsky_fail == 1, Pr(partial_voting_main == 1) is 42.1%

cor_and_condprob(main, "berinsky_fail", "partial_voting_main")
# [1] 0.1790337
# Cond. on partial_voting_main == 1, Pr(berinsky_fail == 1) is 52.6%
# Cond. on berinsky_fail == 1, Pr(partial_voting_main == 1) is 28.6%

### Voting mode anchor question ------------------------------------------------
# cor_and_condprob(main, "ternovsky_fail", "partial_voting_anc")
# cor_and_condprob(main, "berinsky_fail", "partial_voting_anc")

## Between partial rankings? ---------------------------------------------------
## I don't want to do all length(combn(8, 2)) = 56 combinations, so...

### Within Tate 1993 -----------------------------------------------------------
cor_and_condprob(main, "partial_tate_main", "partial_tate_anc")
# Cond. on partial_tate_anc == 1, Pr(partial_tate_main == 1) is 62.5%
# Cond. on partial_tate_main == 1, Pr(partial_tate_anc == 1) is 71.4%

### Within Nelson 1997 ---------------------------------------------------------
cor_and_condprob(main, "partial_nelson_main", "partial_nelson_anc")
# [1] 0.6871226
# Cond. on partial_nelson_anc == 1, Pr(partial_nelson_main == 1) is 81.8%
# Cond. on partial_nelson_main == 1, Pr(partial_nelson_anc == 1) is 64.3%

### Within Identity ------------------------------------------------------------
cor_and_condprob(main, "partial_identity_main", "partial_identity_anc")
# [1] 0.6817068
# Cond. on partial_identity_anc == 1, Pr(partial_identity_main == 1) is 82.4%
# Cond. on partial_identity_main == 1, Pr(partial_identity_anc == 1) is 66.7%

### Within Voting Mode ---------------------------------------------------------
cor_and_condprob(main, "partial_voting_main", "partial_voting_anc")

## Any demographic correlations? -----------------------------------------------

## Duration of completion? -----------------------------------------------------
