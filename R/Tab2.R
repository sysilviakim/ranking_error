source(here::here("R", "FigC10.R"))

# Distribution of responses to the anchor question =============================
tab <- table(main$anc_identity)
## Turn to tibble after name repairing
tab <- as_tibble(tab, .name_repair = "unique") %>%
  rename(Permutation = `...1`, Frequency = n) %>%
  mutate(`Percent (%)` = round(Frequency / sum(Frequency) * 100, 1))

## Export this into two-column table
tab <- cbind(
  tab[1:12, ],
  bind_rows(
    tab[13:nrow(tab), ],
    tibble(Permutation = NA, Frequency = NA, `Percent (%)` = NA)
  )
)

## Table 2, formerly anchor_detail.tex
xtable(
  tab,
  caption = "Distribution of Responses to the Anchor Question",
  label = "tab:anchor_detail",
  align = "llrrlrr"
) %>%
  print(
    type = "latex",
    file = here("tab", "Tab2.tex"),
    include.rownames = FALSE,
    floating = FALSE,
    booktabs = TRUE
  )
