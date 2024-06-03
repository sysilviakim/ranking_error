source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# Grab main data
main <- df_list$main


# Data processing ==============================================================
main <- main %>%
  mutate(
    across(where(is.labelled), ~ as.numeric(as.character(.)))
  )

# Reference set: (party, religion, gender, race)

data <- main %>%
  select(app_identity_1, app_identity_2, app_identity_3, app_identity_4,
         anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
         anc_correct_identity, 
         anc_correct_id_alphabet,
         anc_correct_id_exact,
         app_identity_repeat,
         app_identity,
         weight,
         race4, gender3, pid3, educ4)

table(main$anc_correct_id_alphabet, useNA = "always")
table(main$anc_correct_id_exact, useNA = "always")

# Half receives Alphabet, the other half Exact
table(main$anc_correct_id_alphabet,
      main$anc_correct_id_exact, useNA = "always")

table(main$app_identity_repeat, useNA = "always")


dt_alpha <- data %>% filter(!is.na(anc_correct_id_alphabet))
dt_exact <-data %>% filter(!is.na(anc_correct_id_exact))
dt_repeat <- data %>% filter(!is.na(app_identity_repeat)) %>%
  mutate(repeat_correct = ifelse(app_identity_repeat == app_identity, 1, 0))

table(dt_repeat$repeat_correct)



# I think something is wrong with the following approach
# Especially, anchor_q should be modified
# Yuki will work on this more tomorrow


# Direct bias correction
d <- imprr_direct(
  data = data,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity",
  weight = data$weight
)


# Alphabet anchor
d_alpha <- imprr_direct(
  data = dt_alpha,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_id_alphabet",
  weight = dt_alpha$weight
)


# Exact anchor
d_exact <- imprr_direct(
  data = dt_exact,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_id_exact",
  weight = dt_exact$weight
)


# Repeated question
d_repeat <- imprr_direct(
  data = dt_repeat,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "repeat_correct",
  weight = dt_repeat$weight
)



d$est_p_random
d_alpha$est_p_random
d_exact$est_p_random
d_repeat$est_p_random

d_com <- rbind(d_alpha$est_p_random,
               d_exact$est_p_random,
               d_repeat$est_p_random,
               d$est_p_random
) 
d_com$group <- c("Alphabet anchor", "Exact anchor", "Repeated", "Main anchor")

width_par <- 0.5
ggplot(d_com, aes(x = mean, y = group)) +
  geom_point() +  
  geom_errorbar(aes(xmin = lower, 
                    xmax = upper), width = 0,
                position = position_dodge(width_par), size = 0.6) +
  xlab("Proportion of Random Responses") -> a


dt_main <- d$qoi %>% filter(qoi == "average rank") %>%
  mutate(dt = "Main")
dt_alpha <- d_alpha$qoi %>% filter(qoi == "average rank") %>%
  mutate(dt = "Alphabet")
dt_exact <- d_exact$qoi %>% filter(qoi == "average rank") %>%
  mutate(dt = "Exact")
dt_repeat <- d_repeat$qoi %>% filter(qoi == "average rank") %>%
  mutate(dt = "Repeat")

avg_gg_comb <- rbind(dt_main,
                     dt_alpha,
                     dt_exact,
                     dt_repeat) %>%
  mutate(item = case_when(item == "app_identity_1" ~ "party",
                          item == "app_identity_2" ~ "religion",
                          item == "app_identity_3" ~ "gender",
                          item == "app_identity_4" ~ "race_ethnicity")) %>%
  rename(estimate = mean,
         conf.low = lower,
         conf.high = upper)


avg_gg_comb %>% 
  ggplot(aes(y = fct_reorder(item, -estimate, mean), 
             x = estimate, group = dt, color = dt)) +
  scale_fill_brewer(palette = "Accent") +
  geom_vline(xintercept = 2.5, lty = 2, color = alpha("black", 0.5), linewidth = 0.3) +  
  geom_point(aes(shape = dt), position = position_dodge(width = width_par), size = 1.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0,
                position = position_dodge(width_par), size = 0.6) +
  scale_color_manual(values = c("Main" = "darkcyan", 
                                "Alphabet" = alpha("dimgray", 0.5),
                                "Exact" =  alpha("dimgray", 0.5),
                                "Repeat" = alpha("dimgray", 0.5))) +
  geom_text(aes(x = conf.high + 0.1,
                label = round(estimate, 1.5)),
            position = position_dodge(width = width_par),
            size = 1.5,
            color = "black") +
  # facet_grid(sample ~ .) +
  xlim(0.8, 4.1) +
  ylab("") +
  xlab("Average Rank") +
  theme_classic(base_rect_size = 11/44) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        text = element_text(size = 8)) -> b

ggpubr::ggarrange(a, b, ncol = 1,
                  heights = c(0.5,1))

ggsave(here::here("fig", "weight-avg-rank-multiple-anchors.pdf"), 
       width = 5, height = 5)

