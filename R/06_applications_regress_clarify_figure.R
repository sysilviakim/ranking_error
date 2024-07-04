source(here::here("R", "utilities.R"))
load(here("data", "tidy", "PL_unadjusted.Rda"))
load(here("data", "tidy", "PL_ipw.Rda"))


# 6. Visualize the final results  ==============================================
ggdt_all <- rbind(ggdt_unadjusted, PL_ipw)


p <- ggdt_all %>%
  ggplot(aes(x = ideology, y = mean, color = results, shape = results)) +
  geom_point(position = position_dodge2(width = 0.5)) +
  geom_pointrange(aes(ymin = low, ymax = up),
    position = position_dodge2(width = 0.5)
  ) +
  scale_color_manual(values = c("darkcyan", alpha("dimgray", 0.3))) +
  facet_wrap(~ranking) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  xlab("Ideology (liberal - conservative)") +
  ylab("Predicted Probability") +
  ylim(0, 0.45) +  
  labs(
    caption = paste0(
      "Predictions for a 40-year-old white male ",
      "with some college education,\nindependent, and in Northeast"
    )
  )

pdf_default(p) +
  theme(
    plot.caption = element_text(hjust = 0),
    strip.text.x = element_text(angle = 0, hjust = 0),
    strip.background = element_rect(fill = "white"),
    legend.position = c(0.35, 0.9),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = alpha("lightblue", 0),
      size = 0.5, linetype = "solid"
    )
  )
ggsave(
  here("fig", "placketluce_weight_all_weight-boot.pdf"),
  width = 6, height = 5
)

# First difference between the most liberal and conservative ===================
## Bias-corrected: 0.32752908 - 0.06778887 = 0.2597402
corrected_diff <- ggdt1 %>%
  filter(ideology == 7 | ideology == 1) %>%
  filter(results == "bias-corrected") %>%
  {.$mean[1] - .$mean[2]}
corrected_diff

## Raw data: 0.20329978 - 0.04637296 = 0.1569268
raw_diff <- ggdt1 %>%
  filter(ideology == 7 | ideology == 1) %>%
  filter(results == "raw data") %>%
  {.$mean[1] - .$mean[2]}
raw_diff

corrected_diff / raw_diff

ggdt3 %>%
  group_by(results) %>%
  summarize(mean(mean))
