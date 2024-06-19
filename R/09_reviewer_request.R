# Later, push these codes to the main scripts
source(here::here("R", "affective_polarization", "01_setup.R"))
library(lmtest)
library(sandwich)
library(stargazer)
library(fixest)
library(coefplot)

# What determines a random response? OLS =======================================
temp <- list(
  repeated = lm(
    repeat_identity ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  berinsky_fail = lm(
    berinsky_fail ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  ternovski_fail = lm(
    ternovski_fail ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  anchor_main = lm(
    random_identity ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  exact = lm(
    random_id_exact ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  ),
  alphabet = lm(
    random_id_alphabet ~ pid3final + ideo7 + party_intense + age +
      gender3 + race4labeled + educ4 + faminc + newsint + relig5 + region,
    data = main, weights = weight
  )
) %>%
  coef_rename()

## Export
stargazer(
  temp %>% map("model"),
  se = temp %>% map("se_robust"),
  omit = "Constant", dep.var.labels.include = FALSE,
  header = FALSE, model.numbers = FALSE,
  column.labels = c(
    "Repeated", "Attention I", "Attention II",
    "Anchor main", "Anchor exact", "Anchor alphabet"
  ),
  dep.var.caption = "What Determins a Random Response?",
  out = here("tab", "predict_random_identity.tex"), float = FALSE,
  omit.stat = c("f", "ser"), star.cutoffs = c(0.05, 0.01, 0.001),
  no.space = TRUE
)

## Coefplots
p <- coefplot(
  list(
    temp$repeated$model,
    temp$berinsky_fail$model,
    temp$ternovski_fail$model,
    temp$anchor_main$model,
    temp$exact$model,
    temp$alphabet$model
  )
)

p_final <- p$prms %>%
  mutate(
    Type = case_when(
      id == 1 ~ "Repeated",
      id == 2 ~ "Attention I",
      id == 3 ~ "Attention II",
      id == 4 ~ "Anchor main",
      id == 5 ~ "Anchor exact",
      id == 6 ~ "Anchor alphabet"
    ),
    Type = factor(
      Type, levels = c(
        "Repeated", "Attention I", "Attention II",
        "Anchor main", "Anchor exact", "Anchor alphabet"
      )
    ),
    estimate_names = factor(
      estimate_names, levels = names(temp$repeated$model$coefficients)
    )
  ) %>%
  ## Drop constant
  filter(estimate_names != "Constant") %>%
  ggplot(aes(x = estimate_names, y = y, group = Type, color = Type)) + 
  ## Do not overlay but separate by type
  geom_point(position = position_dodge(width = 0.5)) +
  ## Using ci_low, ci_high, create error bar
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.1, size = 0.5, position = position_dodge(width = 0.5)
  ) + 
  scale_color_viridis_d(end = .85, direction = -1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  xlab("Covariates") + 
  ylab("Coefficients")

pdf_default(p_final) + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  here("fig", "predict_random_identity_coefplot.pdf"),
  width = 9, height = 5
)
