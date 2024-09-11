source(here::here("R", "FigB4.R"))

# Main application and anchor questions ========================================
## Main ranking question of interest (identity) --------------------------------
tab <- table(main$app_identity_recorded) %>% permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## X-sq = 121.593, p-value = 2.149e-15, ES = 0.3352, N = 287+
temp <- table_to_tibble(tab)
p1 <- plot_nolegend(
  plot_dist_ranking(temp, ylim = .15, fill = "darkcyan")
)
plot_nolegend(p1 + theme_bw())

## Figure 6(a), formerly main-identity-recorded.pdf
ggsave(
  here("fig", "Fig6a.pdf"),
  width = 8.2, height = 2.5
)

## Main anchor -----------------------------------------------------------------
## Subgroup: those who passed the main anchor
tab <- main %>%
  filter(anc_correct_identity == 1) %>%
  {table(.$anc_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 0.1066, ES = 0.2050, N = 767+
temp <- table_to_tibble(tab)
pass <- plot_dist_ranking(temp, ylim = .15, fill = "dimgray")
plot_nolegend(pass + theme_bw())

## Figure 6(b), formerly main-anchor-recorded-passers.pdf
ggsave(
  here("fig", "Fig6b.pdf"),
  width = 8.2, height = 2.5
)

## Subgroup: those who failed the main anchor
tab <- main %>%
  filter(anc_correct_identity == 0) %>%
  {table(.$anc_identity_recorded)} %>%
  permn_augment()
round(prop.table(tab) * 100, digits = 1)
chisq_power(tab) ## p-value = 5.793e-13, ES = 0.5737, N = 98+
temp <- table_to_tibble(tab)
fail <- plot_dist_ranking(temp, ylim = .15, fill = "dimgray")
plot_nolegend(fail + theme_bw())

## Figure 6(c), formerly main-anchor-recorded-failers.pdf
ggsave(
  here("fig", "Fig6c.pdf"),
  width = 8.2, height = 2.5
)
