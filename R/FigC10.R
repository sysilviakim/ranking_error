source(here::here("R", "TabC1.R"))

## Coefplots
p <- multiplot(
  `Repeated` = temp$repeated$model,
  `Attention I` = temp$ternovski_fail$model,
  `Attention II` = temp$berinsky_fail$model,
  `Anchor main` = temp$anchor_main$model,
  `Anchor exact` = temp$exact$model,
  `Anchor alphabet` = temp$alphabet$model
)

p_final <- p$data %>%
  mutate(
    Model = factor(
      Model, levels = c(
        "Repeated", "Attention I", "Attention II",
        "Anchor main", "Anchor exact", "Anchor alphabet"
      )
    ),
    Coefficient = factor(
      Coefficient, levels = names(temp$repeated$model$coefficients)
    )
  ) %>%
  ## Drop constant
  filter(Coefficient != "(Intercept)") %>%
  ggplot(aes(x = Coefficient, y = Value, group = Model, color = Model)) + 
  ## Do not overlay but separate by type
  geom_point(position = position_dodge(width = 0.5)) +
  ## Using ci_low, ci_high, create error bar
  geom_errorbar(
    aes(ymin = LowOuter, ymax = HighOuter),
    width = 0.1, linewidth = 0.5, position = position_dodge(width = 0.5)
  ) + 
  scale_color_viridis_d(end = .85, direction = -1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  xlab("Covariates") + 
  ylab("Coefficients")

pdf_default(p_final) + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Figure C.10, formerly predict_random_identity_coefplot.pdf
ggsave(
  here("fig", "FigC10.pdf"),
  width = 9, height = 5
)
