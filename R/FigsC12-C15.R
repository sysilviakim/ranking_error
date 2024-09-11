source(here::here("R", "Fig6.R"))

pass <-
  plot_dist_ranking(temp, ylim = .2, family = "CM Roman", fill = "dimgray")
plot_nolegend(pdf_default(pass))

## Figure C.12(a), formerly main-anchor-recorded-passers-ylim-20.pdf
ggsave(
  here("fig", "FigC12a.pdf"),
  width = 7.5, height = 2.5
)

fail <-
  plot_dist_ranking(temp, ylim = .2, family = "CM Roman", fill = "dimgray")
plot_nolegend(pdf_default(fail))

## Figure C.12(b), formerly main-anchor-recorded-failers-ylim-20.pdf
ggsave(
  here("fig", "FigC12b.pdf"),
  width = 7.5, height = 2.5
)

## Alphabet anchor -------------------------------------------------------------
## Subgroup: those who passed the alphabet anchor
tab <- main %>%
  filter(anc_correct_id_alphabet == 1) %>%
  {table(.$anc_id_alphabet_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.8084, ES = 0.2685, N = 447+
temp <- table_to_tibble(tab)
pass <-
  plot_dist_ranking(temp, ylim = .2, family = "CM Roman", fill = "dimgray")
plot_nolegend(pdf_default(pass))

## Figure C.13(a), formerly alphabet-anchor-recorded-passers.pdf
ggsave(
  here("fig", "FigC13a.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the alphabet anchor
tab <- main %>%
  filter(anc_correct_id_alphabet == 0) %>%
  {table(.$anc_id_alphabet_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.05695, ES = 0.3390, N = 280+
temp <- table_to_tibble(tab)
fail <-
  plot_dist_ranking(temp, ylim = .2, family = "CM Roman", fill = "dimgray")
plot_nolegend(pdf_default(fail))

## Figure C.13(b), formerly alphabet-anchor-recorded-failers.pdf
ggsave(
  here("fig", "FigC13b.pdf"),
  width = 7.5, height = 2.5
)

## Exact anchor ----------------------------------------------------------------
## Subgroup: those who passed the exact anchor
tab <- main %>%
  filter(anc_correct_id_exact == 1) %>%
  {table(.$anc_id_exact_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.4055, ES = 0.2870, N = 391+
temp <- table_to_tibble(tab)
pass <- 
  plot_dist_ranking(temp, ylim = .2, family = "CM Roman", fill = "dimgray")
plot_nolegend(pdf_default(pass))

## Figure C.14(a), formerly exact-anchor-recorded-passers.pdf
ggsave(
  here("fig", "FigC14a.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the exact anchor
tab <- main %>%
  filter(anc_correct_id_exact == 0) %>%
  {table(.$anc_id_exact_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 6.595e-08, ES = 0.5543, N = 105+
temp <- table_to_tibble(tab)
fail <- 
  plot_dist_ranking(temp, ylim = .2, family = "CM Roman", fill = "dimgray")
plot_nolegend(pdf_default(fail))

## Figure C.14(b), formerly exact-anchor-recorded-failers.pdf
ggsave(
  here("fig", "FigC14b.pdf"),
  width = 7.5, height = 2.5
)

## Repeated question -----------------------------------------------------------
## Subgroup: those who passed the repeated question
tab <- main %>%
  filter(repeated_correct == "Passed") %>%
  {table(.$app_identity_repeat_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.01503, ES = 0.3332, N = 290+
temp <- table_to_tibble(tab)
pass <- 
  plot_dist_ranking(temp, ylim = .2, family = "CM Roman", fill = "dimgray")
plot_nolegend(pdf_default(pass))

## Figure C.15(a), formerly repeated-recorded-passers.pdf
ggsave(
  here("fig", "FigC15a.pdf"),
  width = 7.5, height = 2.5
)

## Subgroup: those who failed the repeated question
tab <- main %>%
  filter(repeated_correct == "Failed") %>%
  {table(.$app_identity_repeat_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 3.215e-12, ES = 0.7407, N = 58+
temp <- table_to_tibble(tab)
fail <- 
  plot_dist_ranking(temp, ylim = .2, family = "CM Roman", fill = "dimgray")
plot_nolegend(pdf_default(fail))

## Figure C.15(b), formerly repeated-recorded-failers.pdf
ggsave(
  here("fig", "FigC15b.pdf"),
  width = 7.5, height = 2.5
)
