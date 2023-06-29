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
