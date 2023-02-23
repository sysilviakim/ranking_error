source(here::here("R", "pretest_zero_weak_context.R"))

# Tate (1993) ==================================================================
## Collapse order of randomized item presentation ------------------------------
main <- main %>%
  mutate(
    across(
      contains("tate1993"),
      ~ gsub(
        paste0(
          "Federal government that create policies that affect people's lives at the federal level|",
          "Working in Congress on bills concerning national issues"
        ),
        "1",
        gsub(
          paste0(
            "State government that create policies that affect people's lives at the state level|",
            "Helping people in the district who have personal problems with government"
          ),
          "2",
          gsub(
            paste0(
              "Municipal government that create policies that affect people's lives at the city level|",
              "Making sure the state/district gets its fair share of government money and projects"
            ),
            "3",
            gsub("\\|", "", .x)
          )
        )
      )
    )
  )

## Collapse into ranking pattern -----------------------------------------------
tate1993 <- main %>%
  mutate(
    across(
      anc_tate1993_1:app_tate1993_3,
      ~ case_when(.x == "-99" ~ "9", TRUE ~ .x)
    )
  ) %>%
  unite("anc_tate1993", sep = "", anc_tate1993_1:anc_tate1993_3) %>%
  unite("app_tate1993", sep = "", app_tate1993_1:app_tate1993_3)

rank_main <- tate1993$anc_tate1993
rank_anch <- tate1993$app_tate1993

sum(grepl("9", rank_main)) ## 8
sum(grepl("9", rank_anch)) ## 7

tab_main <- table(rank_main[!grepl("9", rank_main)])
tab_anch <- table(rank_anch[!grepl("9", rank_anch)])

## Some *really* interesting patterns
round(prop.table(tab_main) * 100, digits = 1)
#  123  132  213  231  312  321 
# 57.8  6.7 10.0  5.6  7.8 12.2
round(prop.table(tab_anch) * 100, digits = 1)
#  123  132  213  231  312  321 
#  8.8 35.2  9.9 20.9 15.4  9.9

## Chi-square test and power test ----------------------------------------------
## p-value < 2.2e-16
chisq.test(tab_main, p = rep(1 / length(tab_main), length(tab_main)))
## p-value < 3.413e-05
chisq.test(tab_anch, p = rep(1 / length(tab_anch), length(tab_anch)))

P0 <- rep(1 / length(tab_main), length(tab_main))
P1 <- as.numeric(prop.table(tab_main))
ES.w1(P0, P1) # 1.111(!)
pwr.chisq.test(w = ES.w1(P0, P1), df = (length(tab_main) - 1), power = 0.95)

P0 <- rep(1 / length(tab_anch), length(tab_anch))
P1 <- as.numeric(prop.table(tab_anch))
ES.w1(P0, P1) # 0.556
pwr.chisq.test(w = ES.w1(P0, P1), df = (length(tab_anch) - 1), power = 0.95)

## Visualize -------------------------------------------------------------------
### Anchor question ------------------------------------------------------------
temp <- enframe(tab_main, name = "ranking", value = "freq") %>%
  mutate(
    ranking = factor(ranking),
    freq = as.numeric(freq),
    prop = freq / sum(freq)
  )

p <- temp %>%
  ggplot(aes(x = ranking, y = prop, fill = "1")) +
  geom_col() +
  scale_fill_manual(values = "firebrick4") +
  xlab("Observed Ranking") +
  ylab("") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.65)) +
  geom_hline(yintercept = 1 / factorial(3)) +
  geom_text(
    aes(
      label = paste0(round(prop * 100, digits = 1), "%"),
      family = "CM Roman"
    ),
    vjust = -0.5, size = 3
  )
plot_nolegend(pdf_default(p))
ggsave(here("fig", "pretest-tate1993-main.pdf"), width = 4.5, height = 2.8)

temp <- enframe(tab_anch, name = "ranking", value = "freq") %>%
  mutate(
    ranking = factor(ranking),
    freq = as.numeric(freq),
    prop = freq / sum(freq)
  )

p <- temp %>%
  ggplot(aes(x = ranking, y = prop, fill = "1")) +
  geom_col() +
  scale_fill_manual(values = "firebrick4") +
  xlab("Observed Ranking") +
  ylab("") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.65)) +
  geom_hline(yintercept = 1 / factorial(3)) +
  geom_text(
    aes(
      label = paste0(round(prop * 100, digits = 1), "%"),
      family = "CM Roman"
    ),
    vjust = -0.5, size = 3
  )
plot_nolegend(pdf_default(p))
ggsave(here("fig", "pretest-tate1993-anchor.pdf"), width = 4.5, height = 2.8)
