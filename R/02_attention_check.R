source(here::here("R", "utilities.R"))

# Import data ==================================================================
## substitute file name when actual data is sent
fname <- "AmericanRanking_Interim_June_2023.sav"
df_list <- yougov_import(fname)
main <- df_list$main

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


