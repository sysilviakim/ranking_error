source(here::here("R", "utilities.R"))
df_list <- qualtrics_import("pretest-03.csv")
main <- df_list$main

# Geometric patterning rates ===================================================
main %>%
  select(matches("^ns_")) %>%
  map(~ sum(.x == 1) / nrow(main))

## Tate 1993: 48.0%
## Electoral systems: 84.7%
## Identity: 72.4%
## Polarization: 46.0%

## Yikes.
sort(table(main$anc_e_systems), decreasing = TRUE)
sort(table(main$anc_identity), decreasing = TRUE)

# Correlation between attention filters ========================================
cor_and_condprob(main, "ternovski_fail", "berinsky_fail")
# [1] 0.5763128
# Cond. on berinsky_fail == 1, Pr(ternovski_fail == 1) is 63.3%
# Cond. on ternovski_fail == 1, Pr(berinsky_fail == 1) is 76.0%

# Correlation between attention filters and geometric ==========================
## Tate 1993 -------------------------------------------------------------------
cor_and_condprob(main, "ternovski_fail", "ns_tate")
# [1] 0.2347556
# Cond. on ns_tate == 1, Pr(ternovski_fail == 1) is 36.2%
# Cond. on ternovski_fail == 1, Pr(ns_tate == 1) is 68.0%

cor_and_condprob(main, "berinsky_fail", "ns_tate")
# [1] 0.47031
# Cond. on ns_tate == 1, Pr(berinsky_fail == 1) is 53.2%
# Cond. on berinsky_fail == 1, Pr(ns_tate == 1) is 83.3%

## Electoral systems -----------------------------------------------------------
cor_and_condprob(main, "ternovski_fail", "ns_esystem")
# [1] 0.2487798
# Cond. on ns_esystem == 1, Pr(ternovski_fail == 1) is 30.1%
# Cond. on ternovski_fail == 1, Pr(ns_esystem == 1) is 100.0%

cor_and_condprob(main, "berinsky_fail", "ns_esystem")
# [1] 0.2208731
# Cond. on ns_esystem == 1, Pr(berinsky_fail == 1) is 34.9%
# Cond. on berinsky_fail == 1, Pr(ns_esystem == 1) is 96.7%

## Identity --------------------------------------------------------------------
cor_and_condprob(main, "ternovski_fail", "ns_identity")
# [1] 0.3084845
# Cond. on ns_identity == 1, Pr(ternovski_fail == 1) is 33.8%
# Cond. on ternovski_fail == 1, Pr(ns_identity == 1) is 96.0%

cor_and_condprob(main, "berinsky_fail", "ns_identity")
# [1] 0.2609297
# Cond. on ns_identity == 1, Pr(berinsky_fail == 1) is 38.0%
# Cond. on berinsky_fail == 1, Pr(ns_identity == 1) is 90.0%

## Polarization ----------------------------------------------------------------
cor_and_condprob(main, "ternovski_fail", "ns_polar")
# [1] 0.4002311
# Cond. on ns_polar == 1, Pr(ternovski_fail == 1) is 44.4%
# Cond. on ternovski_fail == 1, Pr(ns_polar == 1) is 80%

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
## Proportions -----------------------------------------------------------------
repeat_coherent(main, "app_tate_1993", "app_tate_repeat") # 52.6%
repeat_coherent(main, "app_e_systems", "app_e_systems_repeat") # 12.8%
repeat_coherent(main, "app_identity", "app_identity_repeat") # 26.5%
repeat_coherent(main, "app_polar", "app_pol_repeat") # 16.3%

## If truncated 1-3? -----------------------------------------------------------
repeat_coherent(main, "app_esystem_trunc3", "app_esystem_trunc3_repeat") # 17.0%
repeat_coherent(main, "app_identity_trunc3", "app_identity_trunc3_repeat") # 32.7%
repeat_coherent(main, "app_polar_trunc3", "app_polar_trunc3_repeat") # 27.9%

repeat_coherent(main, "app_esystem_trunc2", "app_esystem_trunc2_repeat") # 19.1%
repeat_coherent(main, "app_identity_trunc2", "app_identity_trunc2_repeat") # 40.8%
repeat_coherent(main, "app_polar_trunc2", "app_polar_trunc2_repeat") # 34.9%

repeat_coherent(main, "app_tate_trunc1", "app_tate_trunc1_repeat") # 64.9%
repeat_coherent(main, "app_esystem_trunc1", "app_esystem_trunc1_repeat") # 23.4%
repeat_coherent(main, "app_identity_trunc1", "app_identity_trunc1_repeat") # 44.9%
repeat_coherent(main, "app_polar_trunc1", "app_polar_trunc1_repeat") # 41.9%

## Mm... if non-geometric and/or truncated? ---------------------------------------
## 44.4%
repeat_coherent(
  main %>% filter(ns_esystem == 0),
  "app_e_systems", "app_e_systems_repeat"
)

## 44.4%
repeat_coherent(
  main %>% filter(ns_esystem == 0),
  "app_esystem_trunc3", "app_esystem_trunc3_repeat"
)

## 44.4%
repeat_coherent(
  main %>% filter(ns_esystem == 0),
  "app_esystem_trunc2", "app_esystem_trunc2_repeat"
)

## 44.4%
repeat_coherent(
  main %>% filter(ns_esystem == 0),
  "app_esystem_trunc1", "app_esystem_trunc1_repeat"
)

## 46.2%
repeat_coherent(
  main %>% filter(ns_identity == 0),
  "app_identity", "app_identity_repeat"
)

## 61.5%
repeat_coherent(
  main %>% filter(ns_identity == 0),
  "app_identity_trunc3", "app_identity_trunc3_repeat"
)

## 69.2%
repeat_coherent(
  main %>% filter(ns_identity == 0),
  "app_identity_trunc2", "app_identity_trunc2_repeat"
)

## 69.2%
repeat_coherent(
  main %>% filter(ns_identity == 0),
  "app_identity_trunc1", "app_identity_trunc1_repeat"
)

## 30.4%
repeat_coherent(
  main %>% filter(ns_polar == 0),
  "app_polar", "app_pol_repeat"
)

## 47.8%
repeat_coherent(
  main %>% filter(ns_polar == 0),
  "app_polar_trunc3", "app_polar_trunc3_repeat"
)

## 60.9%
repeat_coherent(
  main %>% filter(ns_polar == 0),
  "app_polar_trunc2", "app_polar_trunc2_repeat"
)

## 65.2%
repeat_coherent(
  main %>% filter(ns_polar == 0),
  "app_polar_trunc1", "app_polar_trunc1_repeat"
)

## Correlation with attention filters ------------------------------------------
cor_and_condprob(main, "repeat_tate", "ternovski_fail")
# [1] -0.0589692
# Cond. on ternovski_fail == 1, Pr(repeat_tate == 1) is 20%
# Cond. on repeat_tate == 1, Pr(ternovski_fail == 1) is 18.5%

cor_and_condprob(main, "repeat_tate", "berinsky_fail")
# [1] 0.07139329
# Cond. on berinsky_fail == 1, Pr(repeat_tate == 1) is 26.7%
# Cond. on repeat_tate == 1, Pr(berinsky_fail == 1) is 29.6%

cor_and_condprob(main, "repeat_esystem", "ternovski_fail")
# [1] 0.2491671
# Cond. on ternovski_fail == 1, Pr(repeat_esystem == 1) is 56%
# Cond. on repeat_esystem == 1, Pr(ternovski_fail == 1) is 34.1%

cor_and_condprob(main, "repeat_esystem", "berinsky_fail")
# [1] 0.155278
# Cond. on berinsky_fail == 1, Pr(repeat_esystem == 1) is 53.3%
# Cond. on repeat_esystem == 1, Pr(berinsky_fail == 1) is 39%

cor_and_condprob(main, "repeat_identity", "ternovski_fail")
# [1] 0.3042903
# Cond. on ternovski_fail == 1, Pr(repeat_identity == 1) is 40%
# Cond. on repeat_identity == 1, Pr(ternovski_fail == 1) is 27.8%

cor_and_condprob(main, "repeat_identity", "berinsky_fail")
# [1] 0.4184306
# Cond. on berinsky_fail == 1, Pr(repeat_identity == 1) is 53.3%
# Cond. on repeat_identity == 1, Pr(berinsky_fail == 1) is 44.4%

cor_and_condprob(main, "repeat_polar", "ternovski_fail")
# [1] 0.3063817
# Cond. on ternovski_fail == 1, Pr(repeat_polar == 1) is 56%
# Cond. on repeat_polar == 1, Pr(ternovski_fail == 1) is 38.9%

cor_and_condprob(main, "repeat_polar", "berinsky_fail")
# [1] 0.2743516
# Cond. on berinsky_fail == 1, Pr(repeat_polar == 1) is 40%
# Cond. on repeat_polar == 1, Pr(berinsky_fail == 1) is 33.3%

## Correlation with geometric patterns -----------------------------------------
cor_and_condprob(main, "repeat_tate", "ns_tate")
# [1] 0.159063
# Cond. on ns_tate == 1, Pr(repeat_tate == 1) is 34%
# Cond. on repeat_tate == 1, Pr(ns_tate == 1) is 59.3%

cor_and_condprob(main, "repeat_esystem", "ns_esystem")
# [1] 0.4619811
# Cond. on ns_esystem == 1, Pr(repeat_esystem == 1) is 43.4%
# Cond. on repeat_esystem == 1, Pr(ns_esystem == 1) is 87.8%

cor_and_condprob(main, "repeat_identity", "ns_identity")
# [1] 0.267094
# Cond. on ns_identity == 1, Pr(repeat_identity == 1) is 40.8%
# Cond. on repeat_identity == 1, Pr(ns_identity == 1) is 80.6%

cor_and_condprob(main, "repeat_polar", "ns_polar")
# [1] 0.411196
# Cond. on ns_polar == 1, Pr(repeat_polar == 1) is 44.4%
# Cond. on repeat_polar == 1, Pr(ns_polar == 1) is 55.6%

# (Venn Diagram) ===============================================================
venn_diagram_fill(main, "ternovski_fail", "repeat_tate", "ns_tate")
venn_diagram_fill(main, "berinsky_fail", "repeat_tate", "ns_tate")

venn_diagram_fill(main, "ternovski_fail", "repeat_esystem", "ns_esystem")
venn_diagram_fill(main, "berinsky_fail", "repeat_esystem", "ns_esystem")

venn_diagram_fill(main, "ternovski_fail", "repeat_identity", "ns_tate")
venn_diagram_fill(main, "berinsky_fail", "repeat_identity", "ns_tate")

venn_diagram_fill(main, "ternovski_fail", "repeat_polar", "ns_polar")
venn_diagram_fill(main, "berinsky_fail", "repeat_polar", "ns_polar")

# Partial rankers? =============================================================
## Who are the partial rankers?
## Can inattention predict partial ranking?

# Demographic correlations? ====================================================

# Duration of completion? ======================================================
