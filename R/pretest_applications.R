source(here::here("R", "pretest_zero_weak_context.R"))

# Tate (1993) ==================================================================
## Use order of randomized items to recover observed ranking -------------------
### First, turn text into item numbers from the reference choice set.
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

## Collapse "resulting" ranking ------------------------------------------------
tate1993 <- main %>%
  mutate(
    across(
      anc_tate1993_1:app_tate1993_3,
      ~ case_when(.x == "-99" ~ "9", TRUE ~ .x)
    )
  ) %>%
  unite("anc_tate1993", sep = "", anc_tate1993_1:anc_tate1993_3) %>%
  unite("app_tate1993", sep = "", app_tate1993_1:app_tate1993_3)

## Recover the "observed" ranking ----------------------------------------------
## based on randomized order presentation
tate1993$anc_tate1993_obs <- tate1993 %>%
  separate(anc_tate1993_do, sep = c(1, 2), into = c("V1", "V2", "V3")) %>%
  recov_ref_ranking(rank_var = "anc_tate1993") %>%
  .$ref

tate1993$app_tate1993_obs <- tate1993 %>%
  separate(app_tate1993_do, sep = c(1, 2), into = c("V1", "V2", "V3")) %>%
  recov_ref_ranking(rank_var = "app_tate1993") %>%
  .$ref

## Create frequency tables -----------------------------------------------------
rank_main <- tate1993$anc_tate1993_obs
rank_anch <- tate1993$app_tate1993_obs

sum(grepl("9", rank_main)) ## 8
sum(grepl("9", rank_anch)) ## 7

tab_main <- table(rank_main[!grepl("9", rank_main)])
tab_anch <- table(rank_anch[!grepl("9", rank_anch)])

## Remember, 
round(prop.table(tab_main) * 100, digits = 1)
#  123  132  213  231  312  321 
# 21.1 14.4  8.9 15.6 20.0 20.0
round(prop.table(tab_anch) * 100, digits = 1)
#  123  132  213  231  312  321 
# 19.8 12.1 24.2 12.1 15.4 16.5

## Chi-square test and power test ----------------------------------------------
chisq_power(tab_main) ## p-value = 0.3194, ES = 0.255, N = 300+
chisq_power(tab_anch) ## p-value = 0.3073, ES = 0.257, N = 300+

## Visualize -------------------------------------------------------------------
### Anchor question ------------------------------------------------------------
temp <- table_to_tibble(tab_main)
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest-tate1993-main.pdf"), width = 4.5, height = 2.8)

temp <- table_to_tibble(tab_anch)
plot_nolegend(pdf_default(plot_dist_ranking(temp)))
ggsave(here("fig", "pretest-tate1993-anchor.pdf"), width = 4.5, height = 2.8)
