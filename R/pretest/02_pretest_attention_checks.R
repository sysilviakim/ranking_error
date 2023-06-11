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

## Yikes.
sort(table(main$anc_e_systems), decreasing = TRUE)
sort(table(main$anc_identity), decreasing = TRUE)

# Correlation between attention filters ========================================
cor_and_condprob(main, "ternovsky_fail", "berinsky_fail")
# [1] 0.5763128
# Cond. on berinsky_fail == 1, Pr(ternovsky_fail == 1) is 63.3%
# Cond. on ternovsky_fail == 1, Pr(berinsky_fail == 1) is 76.0%

# Correlation between attention filters and geometric ==========================
## Tate 1993 -------------------------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "ns_tate")
# [1] 0.2347556
# Cond. on ns_tate == 1, Pr(ternovsky_fail == 1) is 36.2%
# Cond. on ternovsky_fail == 1, Pr(ns_tate == 1) is 68.0%

cor_and_condprob(main, "berinsky_fail", "ns_tate")
# [1] 0.47031
# Cond. on ns_tate == 1, Pr(berinsky_fail == 1) is 53.2%
# Cond. on berinsky_fail == 1, Pr(ns_tate == 1) is 83.3%

## Electoral systems -----------------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "ns_esystem")
# [1] 0.2487798
# Cond. on ns_esystem == 1, Pr(ternovsky_fail == 1) is 30.1%
# Cond. on ternovsky_fail == 1, Pr(ns_esystem == 1) is 100.0%

cor_and_condprob(main, "berinsky_fail", "ns_esystem")
# [1] 0.2208731
# Cond. on ns_esystem == 1, Pr(berinsky_fail == 1) is 34.9%
# Cond. on berinsky_fail == 1, Pr(ns_esystem == 1) is 96.7%

## Identity --------------------------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "ns_identity")
# [1] 0.3084845
# Cond. on ns_identity == 1, Pr(ternovsky_fail == 1) is 33.8%
# Cond. on ternovsky_fail == 1, Pr(ns_identity == 1) is 96.0%

cor_and_condprob(main, "berinsky_fail", "ns_identity")
# [1] 0.2609297
# Cond. on ns_identity == 1, Pr(berinsky_fail == 1) is 38.0%
# Cond. on berinsky_fail == 1, Pr(ns_identity == 1) is 90.0%

## Polarization ----------------------------------------------------------------
cor_and_condprob(main, "ternovsky_fail", "ns_polar")
# [1] 0.4002311
# Cond. on ns_polar == 1, Pr(ternovsky_fail == 1) is 44.4%
# Cond. on ternovsky_fail == 1, Pr(ns_polar == 1) is 80%

cor_and_condprob(main, "berinsky_fail", "ns_polar")
# [1] 0.4098351
# Cond. on ns_polar == 1, Pr(berinsky_fail == 1) is 51.1%
# Cond. on berinsky_fail == 1, Pr(ns_polar == 1) is 76.7%

# Between geometric responses? =================================================
## Tate x ESystems
cor_and_condprob(main, "ns_tate", "ns_esystem")
# [1] 0.294645
# Cond. on ns_esystem == 1, Pr(ns_tate == 1) is 54.2%
# Cond. on ns_tate == 1, Pr(ns_esystem == 1) is 95.7%

## Tate x Identity
cor_and_condprob(main, "ns_tate", "ns_identity")
# [1] 0.3634062
# Cond. on ns_identity == 1, Pr(ns_tate == 1) is 59.2%
# Cond. on ns_tate == 1, Pr(ns_identity == 1) is 89.4%

## Tate x Polarization
cor_and_condprob(main, "ns_tate", "ns_polar")
# [1] 0.4680065
# Cond. on ns_polar == 1, Pr(ns_tate == 1) is 73.3%
# Cond. on ns_tate == 1, Pr(ns_polar == 1) is 70.2%

## ESystems x Identity
cor_and_condprob(main, "ns_esystem", "ns_identity")
# [1] 0.4356316
# Cond. on ns_identity == 1, Pr(ns_esystem == 1) is 94.4%
# Cond. on ns_esystem == 1, Pr(ns_identity == 1) is 80.7%

## ESystems x Polarization
cor_and_condprob(main, "ns_esystem", "ns_polar")
# [1] 0.3348474
# Cond. on ns_polar == 1, Pr(ns_esystem == 1) is 97.8%
# Cond. on ns_esystem == 1, Pr(ns_polar == 1) is 53%

## Identity x Polarization
cor_and_condprob(main, "ns_identity", "ns_polar")
# [1] 0.4765615
# Cond. on ns_polar == 1, Pr(ns_identity == 1) is 95.6%
# Cond. on ns_identity == 1, Pr(ns_polar == 1) is 60.6%

# Repeated answers? ============================================================
repeat_coherent(main, "app_tate_1993", "app_tate_repeat") ## 52.6%
repeat_coherent(main, "app_e_systems", "app_e_systems_repeat") ## 12.8% 
repeat_coherent(main, "app_identity", "app_identity_repeat") ## 26.5%
repeat_coherent(main, "app_polar", "app_pol_repeat") ## 16.3%

# Partial rankers? =============================================================
## Who are the partial rankers?
## Can inattention predict partial ranking?

# Demographic correlations? ====================================================

# Duration of completion? ======================================================
