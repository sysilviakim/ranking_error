source(here::here("R", "utilities.R"))

# Import data ==================================================================
## substitute file name when actual data is sent
fname <- "AmericanRanking_June_2023.sav"
if (!file.exists(here("data", "tidy", "df_list.Rda"))) {
  df_list <- yougov_import(fname)
  save(df_list, file = here("data", "tidy", "df_list.Rda"))
} else {
  load(here("data", "tidy", "df_list.Rda"))
}
main <- df_list$main

# What are the raw proportion of passing the test? =============================
prop(main, "ternovski_fail") ## 86.3%
prop(main, "berinsky_fail") ## 85.5%

## Pr(c = 1) (correct answer)
## random_* variables 1 = random, 0 = non-random
prop(main, "random_tate") ## 67.9%
prop(main, "random_identity") ## 69.7%
prop(main, "random_id_alphabet", useNA = "no") ## 43.9%
prop(main, "random_id_exact", useNA = "no") ## 53.4%
prop(main, "random_polar") ## 79.7%
prop(main, "random_esystems") ## 56.3%
prop(main, "random_es_alphabet", useNA = "no") ## 69.0%
prop(main, "random_es_temporal", useNA = "no") ## 59.6%

prop(main, "repeat_tate", useNA = "no") ## 66.3%
prop(main, "repeat_identity", useNA = "no") ## 65.6%
prop(main, "repeat_polar", useNA = "no") ## 53.6%
prop(main, "repeat_esystems", useNA = "no") ## 36.5%

## Unbiased estimator for the proportion of non-random responses?
## 67.9% --> 61.5%
unbiased_prop(
  sum(main$random_tate == 0) / sum(!is.na(main$random_tate)),
  J = 3
)
## 69.7% --> 68.4%
unbiased_prop(
  sum(main$random_identity == 0) / sum(!is.na(main$random_identity)),
  J = 4
)
## 79.7% --> 79.5%
unbiased_prop(
  sum(main$random_polar == 0) / sum(!is.na(main$random_polar)),
  J = 5
)
## 56.3% --> 56.2%
unbiased_prop(
  sum(main$random_esystems == 0) / sum(!is.na(main$random_esystems)),
  J = 6
)

# Correlation between attention filters ========================================
## Between attention filters
cor_and_condprob(main, "ternovski_fail", "berinsky_fail")

## Ternovski test x random responses
cor_and_condprob(main, "ternovski_fail", "random_tate")
cor_and_condprob(main, "ternovski_fail", "random_identity")
cor_and_condprob(main, "ternovski_fail", "random_id_alphabet")
cor_and_condprob(main, "ternovski_fail", "random_id_exact")
cor_and_condprob(main, "ternovski_fail", "random_polar")
cor_and_condprob(main, "ternovski_fail", "random_esystems")
cor_and_condprob(main, "ternovski_fail", "random_es_alphabet")
cor_and_condprob(main, "ternovski_fail", "random_es_temporal")

## Berinsky test x random responses
cor_and_condprob(main, "berinsky_fail", "random_tate")
cor_and_condprob(main, "berinsky_fail", "random_identity")
cor_and_condprob(main, "berinsky_fail", "random_id_alphabet")
cor_and_condprob(main, "berinsky_fail", "random_id_exact")
cor_and_condprob(main, "berinsky_fail", "random_polar")
cor_and_condprob(main, "berinsky_fail", "random_esystems")
cor_and_condprob(main, "berinsky_fail", "random_es_alphabet")
cor_and_condprob(main, "berinsky_fail", "random_es_temporal")

## Between random responses (within topic)
cor_and_condprob(main, "random_identity", "random_id_alphabet")
cor_and_condprob(main, "random_identity", "random_id_exact")
cor_and_condprob(main, "random_esystems", "random_es_alphabet")
cor_and_condprob(main, "random_esystems", "random_es_temporal")

## Ternovski test x repeated responses
cor_and_condprob(main, "ternovski_fail", "repeat_tate")
cor_and_condprob(main, "ternovski_fail", "repeat_identity")
cor_and_condprob(main, "ternovski_fail", "repeat_polar")
cor_and_condprob(main, "ternovski_fail", "repeat_esystems")

## Berinsky test x repeated responses
cor_and_condprob(main, "berinsky_fail", "repeat_tate")
cor_and_condprob(main, "berinsky_fail", "repeat_identity")
cor_and_condprob(main, "berinsky_fail", "repeat_polar")
cor_and_condprob(main, "berinsky_fail", "repeat_esystems")

## Random responses x repeated responses (within topic)
cor_and_condprob(main, "random_tate", "repeat_tate")
cor_and_condprob(main, "random_identity", "repeat_identity")
cor_and_condprob(main, "random_id_alphabet", "repeat_identity")
cor_and_condprob(main, "random_id_exact", "repeat_identity")
cor_and_condprob(main, "random_polar", "repeat_polar")
cor_and_condprob(main, "random_esystems", "repeat_esystems")
cor_and_condprob(main, "random_es_alphabet", "repeat_esystems")
cor_and_condprob(main, "random_es_temporal", "repeat_esystems")

# Venn Diagram =================================================================
venn_diagram_fill(main, "ternovski_fail", "repeat_tate", "random_tate")
venn_diagram_fill(main, "berinsky_fail", "repeat_tate", "random_tate")

venn_diagram_fill(main, "ternovski_fail", "repeat_esystems", "random_esystems")
venn_diagram_fill(main, "berinsky_fail", "repeat_esystems", "random_esystems")

venn_diagram_fill(main, "ternovski_fail", "repeat_identity", "random_tate")
venn_diagram_fill(main, "berinsky_fail", "repeat_identity", "random_tate")

venn_diagram_fill(main, "ternovski_fail", "repeat_polar", "random_polar")
venn_diagram_fill(main, "berinsky_fail", "repeat_polar", "random_polar")

# Pattern comparison by attention checks =======================================
berinsky_plots <- pattern_compare_pass_fail(main, "berinsky_fail")
ternovski_plots <- pattern_compare_pass_fail(main, "ternovski_fail")

print(ggarrange(plotlist = berinsky_plots$tate))
ggsave(
  here("fig", "berinsky_fail_recorded_tate.pdf"),
  width = 6, height = 3
)

print(ggarrange(plotlist = ternovski_plots$tate))
ggsave(
  here("fig", "ternovski_fail_recorded_tate.pdf"),
  width = 6, height = 3
)

print(
  ggarrange(
    plotlist = berinsky_plots$identity %>%
      map(
        ~ .x +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      ),
    ncol = 1
  )
)
ggsave(
  here("fig", "berinsky_fail_recorded_identity.pdf"),
  width = 6, height = 3
)

print(
  ggarrange(
    plotlist = ternovski_plots$identity %>%
      map(
        ~ .x +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      ),
    ncol = 1
  )
)
ggsave(
  here("fig", "ternovski_fail_recorded_identity.pdf"),
  width = 6, height = 3
)

print(ggarrange(plotlist = berinsky_plots$no_context_3))
ggsave(
  here("fig", "berinsky_fail_recorded_nocontext3.pdf"),
  width = 6, height = 3
)

print(ggarrange(plotlist = ternovski_plots$no_context_3))
ggsave(
  here("fig", "ternovski_fail_recorded_nocontext3.pdf"),
  width = 6, height = 3
)

print(
  ggarrange(
    plotlist = berinsky_plots$no_context_4 %>%
      map(
        ~ .x +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      ),
    ncol = 1
  )
)
ggsave(
  here("fig", "berinsky_fail_recorded_nocontext4.pdf"),
  width = 6, height = 3
)

print(
  ggarrange(
    plotlist = ternovski_plots$no_context_4 %>%
      map(
        ~ .x +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      ),
    ncol = 1
  )
)
ggsave(
  here("fig", "ternovski_fail_recorded_nocontext4.pdf"),
  width = 6, height = 3
)

# [1] 0.2483813
# Cond. on berinsky_fail == 1, Pr(ternovski_fail == 1) is 34.4%
# Cond. on ternovski_fail == 1, Pr(berinsky_fail == 1) is 36.5%
# [1] 0.1241025
# Cond. on random_tate == 1, Pr(ternovski_fail == 1) is 19.9%
# Cond. on ternovski_fail == 1, Pr(random_tate == 1) is 46.6%
# [1] 0.1470883
# Cond. on random_identity == 1, Pr(ternovski_fail == 1) is 21.3%
# Cond. on ternovski_fail == 1, Pr(random_identity == 1) is 47.3%
# [1] 0.172759
# Cond. on random_id_alphabet == 1, Pr(ternovski_fail == 1) is 19.3%
# Cond. on ternovski_fail == 1, Pr(random_id_alphabet == 1) is 39.2%
# [1] 0.1941519
# Cond. on random_id_exact == 1, Pr(ternovski_fail == 1) is 20.5%
# Cond. on ternovski_fail == 1, Pr(random_id_exact == 1) is 35.1%
# [1] 0.1798178
# Cond. on random_polar == 1, Pr(ternovski_fail == 1) is 25.9%
# Cond. on ternovski_fail == 1, Pr(random_polar == 1) is 38.5%
# [1] 0.1371913
# Cond. on random_esystems == 1, Pr(ternovski_fail == 1) is 19%
# Cond. on ternovski_fail == 1, Pr(random_esystems == 1) is 60.8%
# [1] 0.1717841
# Cond. on random_es_alphabet == 1, Pr(ternovski_fail == 1) is 21.8%
# Cond. on ternovski_fail == 1, Pr(random_es_alphabet == 1) is 24.3%
# [1] 0.1117112
# Cond. on random_es_temporal == 1, Pr(ternovski_fail == 1) is 18.9%
# Cond. on ternovski_fail == 1, Pr(random_es_temporal == 1) is 28.4%
# [1] 0.2004261
# Cond. on random_tate == 1, Pr(berinsky_fail == 1) is 24.8%
# Cond. on berinsky_fail == 1, Pr(random_tate == 1) is 54.8%
# [1] 0.2192757
# Cond. on random_identity == 1, Pr(berinsky_fail == 1) is 26.2%
# Cond. on berinsky_fail == 1, Pr(random_identity == 1) is 54.8%
# [1] 0.1946443
# Cond. on random_id_alphabet == 1, Pr(berinsky_fail == 1) is 20.6%
# Cond. on berinsky_fail == 1, Pr(random_id_alphabet == 1) is 39.5%
# [1] 0.2108547
# Cond. on random_id_exact == 1, Pr(berinsky_fail == 1) is 22.4%
# Cond. on berinsky_fail == 1, Pr(random_id_exact == 1) is 36.3%
# [1] 0.2808623
# Cond. on random_polar == 1, Pr(berinsky_fail == 1) is 34.1%
# Cond. on berinsky_fail == 1, Pr(random_polar == 1) is 47.8%
# [1] 0.2399974
# Cond. on random_esystems == 1, Pr(berinsky_fail == 1) is 24.1%
# Cond. on berinsky_fail == 1, Pr(random_esystems == 1) is 72.6%
# [1] 0.2539862
# Cond. on random_es_alphabet == 1, Pr(berinsky_fail == 1) is 29.1%
# Cond. on berinsky_fail == 1, Pr(random_es_alphabet == 1) is 30.6%
# [1] 0.1914317
# Cond. on random_es_temporal == 1, Pr(berinsky_fail == 1) is 21.6%
# Cond. on berinsky_fail == 1, Pr(random_es_temporal == 1) is 30.6%
# [1] 0.2112265
# Cond. on random_id_alphabet == 1, Pr(random_identity == 1) is 37.5%
# Cond. on random_identity == 1, Pr(random_id_alphabet == 1) is 34.5%
# [1] 0.2836382
# Cond. on random_id_exact == 1, Pr(random_identity == 1) is 45.7%
# Cond. on random_identity == 1, Pr(random_id_exact == 1) is 35.4%
# [1] 0.423808
# Cond. on random_es_alphabet == 1, Pr(random_esystems == 1) is 74.5%
# Cond. on random_esystems == 1, Pr(random_es_alphabet == 1) is 26%
# [1] 0.2083223
# Cond. on random_es_temporal == 1, Pr(random_esystems == 1) is 56.8%
# Cond. on random_esystems == 1, Pr(random_es_temporal == 1) is 26.6%
# [1] 0.08910355
# Cond. on repeat_tate == 1, Pr(ternovski_fail == 1) is 18.8%
# Cond. on ternovski_fail == 1, Pr(repeat_tate == 1) is 22.3%
# [1] 0.1731812
# Cond. on repeat_identity == 1, Pr(ternovski_fail == 1) is 21.2%
# Cond. on ternovski_fail == 1, Pr(repeat_identity == 1) is 27%
# [1] 0.06668797
# Cond. on repeat_polar == 1, Pr(ternovski_fail == 1) is 17.5%
# Cond. on ternovski_fail == 1, Pr(repeat_polar == 1) is 29.7%
# [1] 0.123865
# Cond. on repeat_esystems == 1, Pr(ternovski_fail == 1) is 15.4%
# Cond. on ternovski_fail == 1, Pr(repeat_esystems == 1) is 36.5%
# [1] 0.1632269
# Cond. on repeat_tate == 1, Pr(berinsky_fail == 1) is 22.2%
# Cond. on berinsky_fail == 1, Pr(repeat_tate == 1) is 24.8%
# [1] 0.2502592
# Cond. on repeat_identity == 1, Pr(berinsky_fail == 1) is 27%
# Cond. on berinsky_fail == 1, Pr(repeat_identity == 1) is 32.5%
# [1] 0.2138874
# Cond. on repeat_polar == 1, Pr(berinsky_fail == 1) is 24.3%
# Cond. on berinsky_fail == 1, Pr(repeat_polar == 1) is 38.9%
# [1] 0.1849397
# Cond. on repeat_esystems == 1, Pr(berinsky_fail == 1) is 18%
# Cond. on berinsky_fail == 1, Pr(repeat_esystems == 1) is 40.1%
# [1] 0.2833433
# Cond. on repeat_tate == 1, Pr(random_tate == 1) is 48.3%
# Cond. on random_tate == 1, Pr(repeat_tate == 1) is 24.5%
# [1] 0.2649168
# Cond. on repeat_identity == 1, Pr(random_identity == 1) is 47.6%
# Cond. on random_identity == 1, Pr(repeat_identity == 1) is 27.4%
# [1] 0.1346784
# Cond. on repeat_identity == 1, Pr(random_id_alphabet == 1) is 31.7%
# Cond. on random_id_alphabet == 1, Pr(repeat_identity == 1) is 19.9%
# [1] 0.1255438
# Cond. on repeat_identity == 1, Pr(random_id_exact == 1) is 27%
# Cond. on random_id_exact == 1, Pr(repeat_identity == 1) is 20.1%
# [1] 0.3039608
# Cond. on repeat_polar == 1, Pr(random_polar == 1) is 36.7%
# Cond. on random_polar == 1, Pr(repeat_polar == 1) is 41.8%
# [1] 0.247155
# Cond. on repeat_esystems == 1, Pr(random_esystems == 1) is 52.3%
# Cond. on random_esystems == 1, Pr(repeat_esystems == 1) is 38.7%
# [1] 0.2543388
# Cond. on repeat_esystems == 1, Pr(random_es_alphabet == 1) is 19.4%
# Cond. on random_es_alphabet == 1, Pr(repeat_esystems == 1) is 41.2%
# [1] 0.2473715
# Cond. on repeat_esystems == 1, Pr(random_es_temporal == 1) is 22.6%
# Cond. on random_es_temporal == 1, Pr(repeat_esystems == 1) is 35.6%