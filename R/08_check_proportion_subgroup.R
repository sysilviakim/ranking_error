source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))
library(RColorBrewer)
load(here("data", "tidy", "bias_correction.Rda"))
main <- df_list$main %>%
  mutate(across(where(is.labelled), ~ as.numeric(as.character(.))))

# Data processing ==============================================================
# Reference set: (party, religion, gender, race)
data <- main %>%
  select(
    app_identity_1, app_identity_2, app_identity_3, app_identity_4,
    anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
    anc_correct_identity, weight, race4, gender3, pid3, educ4
  )

# Race =========================================================================
d_white <- imprr_direct(
  data = data %>% filter(race4 == 1),
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 1000
)

d_black <- imprr_direct(
  data = data %>% filter(race4 == 2),
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 1000
)

d_latino <- imprr_direct(
  data = data %>% filter(race4 == 3),
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 1000
)

d_others <- imprr_direct(
  data = data %>% filter(race4 == 4),
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  n_bootstrap = 1000
)

main_direct$est_p_random
# mean     lower     upper
# 1 0.3153146 0.2864261 0.3481958

d_white$est_p_random
# mean     lower     upper
# 1 0.2932953 0.2639125 0.3296256

d_black$est_p_random
# mean     lower     upper
# 1 0.4485607 0.3685907 0.5397301

d_latino$est_p_random
# mean     lower     upper
# 1 0.3547826 0.2879299 0.4282933

d_others$est_p_random
# mean     lower     upper
# 1 0.281966 0.1814745 0.3784499

d_com <- rbind(
  main_direct$est_p_random,
  d_white$est_p_random,
  d_black$est_p_random,
  d_latino$est_p_random,
  d_others$est_p_random
)
d_com$group <- c("sample", "white", "black", "latino", "others")

ggplot(d_com, aes(x = mean, y = group)) +
  geom_point() +
  geom_errorbar(
    aes(
      xmin = lower,
      xmax = upper
    ),
    width = 0,
    position = position_dodge(width = .5), size = 0.6
  ) +
  xlab("Proportion of Random Responses")

ggsave(
  here("fig", "check-pz-race.pdf"),
  width = 4 * 1.2, height = 4
)
