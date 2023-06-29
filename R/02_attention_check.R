source(here::here("R", "utilities.R"))

# Import data ==================================================================
## substitute file name when actual data is sent
fname <- "AmericanRanking_Interim_June_2023.sav"
if (!file.exists(here("data", "tidy", "df_list.Rda"))) {
  df_list <- yougov_import(fname)
  save(df_list, file = here("data", "tidy", "df_list.Rda"))
} else {
  load(here("data", "tidy", "df_list.Rda"))
}
main <- df_list$main

# What are the raw proportion of passing the test? =============================
prop(main, "ternovski_fail")                   ## 85.1%
prop(main, "berinsky_fail")                    ## 83.5%

prop(main, "random_tate")                      ## 64.5%
prop(main, "random_identity")                  ## 66.8%
prop(main, "random_id_alphabet", useNA = "no") ## 44.2%
prop(main, "random_id_exact", useNA = "no")    ## 49.3%
prop(main, "random_polar")                     ## 77.1%
prop(main, "random_esystems")                  ## 53.7%
prop(main, "random_es_alphabet", useNA = "no") ## 66.2%
prop(main, "random_es_temporal", useNA = "no") ## 57.3%

prop(main, "repeat_tate", useNA = "no")        ## 62.7%
prop(main, "repeat_identity", useNA = "no")    ## 62.8%
prop(main, "repeat_polar", useNA = "no")       ## 50.9%
prop(main, "repeat_esystems", useNA = "no")    ## 35.5%

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

# [1] 0.3176585
# Cond. on berinsky_fail == 1, Pr(ternovski_fail == 1) is 40.4%
# Cond. on ternovski_fail == 1, Pr(berinsky_fail == 1) is 44.7%
# [1] 0.1612602
# Cond. on random_tate == 1, Pr(ternovski_fail == 1) is 22.7%
# Cond. on ternovski_fail == 1, Pr(random_tate == 1) is 53.9%
# [1] 0.1720154
# Cond. on random_identity == 1, Pr(ternovski_fail == 1) is 23.6%
# Cond. on ternovski_fail == 1, Pr(random_identity == 1) is 52.5%
# [1] 0.1583064
# Cond. on random_id_alphabet == 1, Pr(ternovski_fail == 1) is 19.3%
# Cond. on ternovski_fail == 1, Pr(random_id_alphabet == 1) is 36.2%
# [1] 0.2341973
# Cond. on random_id_exact == 1, Pr(ternovski_fail == 1) is 23.8%
# Cond. on ternovski_fail == 1, Pr(random_id_exact == 1) is 40.4%
# [1] 0.2245339
# Cond. on random_polar == 1, Pr(ternovski_fail == 1) is 29.6%
# Cond. on ternovski_fail == 1, Pr(random_polar == 1) is 45.4%
# [1] 0.1652626
# Cond. on random_esystems == 1, Pr(ternovski_fail == 1) is 21.3%
# Cond. on ternovski_fail == 1, Pr(random_esystems == 1) is 66%
# [1] 0.2218898
# Cond. on random_es_alphabet == 1, Pr(ternovski_fail == 1) is 26.8%
# Cond. on ternovski_fail == 1, Pr(random_es_alphabet == 1) is 29.8%
# [1] 0.1024154
# Cond. on random_es_temporal == 1, Pr(ternovski_fail == 1) is 18.5%
# Cond. on ternovski_fail == 1, Pr(random_es_temporal == 1) is 27%
# [1] 0.2720568
# Cond. on random_tate == 1, Pr(berinsky_fail == 1) is 30.1%
# Cond. on berinsky_fail == 1, Pr(random_tate == 1) is 64.7%
# [1] 0.2864146
# Cond. on random_identity == 1, Pr(berinsky_fail == 1) is 31.6%
# Cond. on berinsky_fail == 1, Pr(random_identity == 1) is 63.5%
# [1] 0.2308334
# Cond. on random_id_alphabet == 1, Pr(berinsky_fail == 1) is 23.1%
# Cond. on berinsky_fail == 1, Pr(random_id_alphabet == 1) is 39.1%
# [1] 0.2283639
# Cond. on random_id_exact == 1, Pr(berinsky_fail == 1) is 25.9%
# Cond. on berinsky_fail == 1, Pr(random_id_exact == 1) is 39.7%
# [1] 0.3687182
# Cond. on random_polar == 1, Pr(berinsky_fail == 1) is 41.7%
# Cond. on berinsky_fail == 1, Pr(random_polar == 1) is 57.7%
# [1] 0.2962079
# Cond. on random_esystems == 1, Pr(berinsky_fail == 1) is 28.4%
# Cond. on berinsky_fail == 1, Pr(random_esystems == 1) is 79.5%
# [1] 0.3547062
# Cond. on random_es_alphabet == 1, Pr(berinsky_fail == 1) is 35.7%
# Cond. on berinsky_fail == 1, Pr(random_es_alphabet == 1) is 35.9%
# [1] 0.23084
# Cond. on random_es_temporal == 1, Pr(berinsky_fail == 1) is 25.9%
# Cond. on berinsky_fail == 1, Pr(random_es_temporal == 1) is 34%
# [1] 0.2551209
# Cond. on random_id_alphabet == 1, Pr(random_identity == 1) is 42%
# Cond. on random_identity == 1, Pr(random_id_alphabet == 1) is 35.5%
# [1] 0.3011365
# Cond. on random_id_exact == 1, Pr(random_identity == 1) is 49%
# Cond. on random_identity == 1, Pr(random_id_exact == 1) is 37.4%
# [1] 0.449361
# Cond. on random_es_alphabet == 1, Pr(random_esystems == 1) is 78.3%
# Cond. on random_esystems == 1, Pr(random_es_alphabet == 1) is 28.1%
# [1] 0.2407266
# Cond. on random_es_temporal == 1, Pr(random_esystems == 1) is 59.5%
# Cond. on random_esystems == 1, Pr(random_es_temporal == 1) is 27.9%
# [1] 0.1662184
# Cond. on repeat_tate == 1, Pr(ternovski_fail == 1) is 24.2%
# Cond. on ternovski_fail == 1, Pr(repeat_tate == 1) is 28.4%
# [1] 0.1814794
# Cond. on repeat_identity == 1, Pr(ternovski_fail == 1) is 22.7%
# Cond. on ternovski_fail == 1, Pr(repeat_identity == 1) is 29.1%
# [1] 0.1319604
# Cond. on repeat_polar == 1, Pr(ternovski_fail == 1) is 19.7%
# Cond. on ternovski_fail == 1, Pr(repeat_polar == 1) is 32.6%
# [1] 0.1298967
# Cond. on repeat_esystems == 1, Pr(ternovski_fail == 1) is 17.6%
# Cond. on ternovski_fail == 1, Pr(repeat_esystems == 1) is 39%
# [1] 0.2417485
# Cond. on repeat_tate == 1, Pr(berinsky_fail == 1) is 27.3%
# Cond. on berinsky_fail == 1, Pr(repeat_tate == 1) is 28.8%
# [1] 0.3292162
# Cond. on repeat_identity == 1, Pr(berinsky_fail == 1) is 33.7%
# Cond. on berinsky_fail == 1, Pr(repeat_identity == 1) is 39.1%
# [1] 0.2426608
# Cond. on repeat_polar == 1, Pr(berinsky_fail == 1) is 26.6%
# Cond. on berinsky_fail == 1, Pr(repeat_polar == 1) is 39.7%
# [1] 0.2225142
# Cond. on repeat_esystems == 1, Pr(berinsky_fail == 1) is 21.5%
# Cond. on berinsky_fail == 1, Pr(repeat_esystems == 1) is 42.9%
# [1] 0.3444331
# Cond. on repeat_tate == 1, Pr(random_tate == 1) is 54.5%
# Cond. on random_tate == 1, Pr(repeat_tate == 1) is 26.9%
# [1] 0.315108
# Cond. on repeat_identity == 1, Pr(random_identity == 1) is 53%
# Cond. on random_identity == 1, Pr(repeat_identity == 1) is 30.7%
# [1] 0.177041
# Cond. on repeat_identity == 1, Pr(random_id_alphabet == 1) is 32%
# Cond. on random_id_alphabet == 1, Pr(repeat_identity == 1) is 22%
# [1] 0.1559984
# Cond. on repeat_identity == 1, Pr(random_id_exact == 1) is 30.9%
# Cond. on random_id_exact == 1, Pr(repeat_identity == 1) is 23.4%
# [1] 0.3638615
# Cond. on repeat_polar == 1, Pr(random_polar == 1) is 41.6%
# Cond. on random_polar == 1, Pr(repeat_polar == 1) is 44.9%
# [1] 0.2904052
# Cond. on repeat_esystems == 1, Pr(random_esystems == 1) is 55.8%
# Cond. on random_esystems == 1, Pr(repeat_esystems == 1) is 39.8%
# [1] 0.2645219
# Cond. on repeat_esystems == 1, Pr(random_es_alphabet == 1) is 21.2%
# Cond. on random_es_alphabet == 1, Pr(repeat_esystems == 1) is 42%
# [1] 0.2917813
# Cond. on repeat_esystems == 1, Pr(random_es_temporal == 1) is 24%
# Cond. on random_es_temporal == 1, Pr(repeat_esystems == 1) is 36.6%
