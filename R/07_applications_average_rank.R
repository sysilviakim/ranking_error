source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# # library(questionr) # for wtd.table()
# library(RColorBrewer)  

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
         anc_correct_identity, weight)

# Direct bias correction
d <- imprr_direct(
  data = data,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity",
  weight = data$weight
)

# Inverse probability weighting
w <- imprr_weights(
  data = data,
  J = 4,
  main_q = "app_identity",
  anc_correct = "anc_correct_identity"
)

print(d)
print(w)

# Downsize data
dt <- main %>%
  mutate(
    party = app_identity_1,
    religion = app_identity_2,
    gender = app_identity_3,
    race_ethnicity = app_identity_4,
    ranking = app_identity
  ) %>%
  left_join(w$weights, by = "ranking") %>%
  mutate(dual_weight = weight * w) %>%
  select(
    party, religion, gender, race_ethnicity, race, ranking, weight, dual_weight) 



# Bias correction ==============================================================

# Direct Bias Correction
avg_rank.w <- d$qoi %>% 
  filter(qoi == "average rank") %>%
  ungroup(qoi) %>%
  mutate(item = case_when(item == "app_identity_1" ~ "party",
                          item == "app_identity_2" ~ "religion",
                          item == "app_identity_3" ~ "gender",
                          item == "app_identity_4" ~ "race_ethnicity"),
         estimate = mean,
         conf.low = lower,
         conf.high = upper,
         dt = "Direct") %>%
  select(item, estimate, conf.low, conf.high, dt)


# IPW
avg_rank.i <- as.data.frame(NA)
avg_rank.i <- lm_robust(party ~ 1, dt, weights = dual_weight) %>% tidy()
avg_rank.i <- rbind(avg_rank.i, lm_robust(religion ~ 1, dt, weights = dual_weight) %>% tidy())
avg_rank.i <- rbind(avg_rank.i, lm_robust(gender ~ 1, dt, weights = dual_weight) %>% tidy())
avg_rank.i <- rbind(avg_rank.i, lm_robust(race_ethnicity ~ 1, dt, weights = dual_weight) %>% tidy())
avg_rank.i$dt <- "IPW"
avg_rank.i <- avg_rank.i %>%
  rename(item = outcome) %>%
  select(item, estimate, conf.low, conf.high, dt)


# Raw Data
avg_rank <- as.data.frame(NA)
avg_rank <- lm_robust(party ~ 1, dt, weights = weight) %>% tidy()
avg_rank <- rbind(avg_rank, lm_robust(religion ~ 1, dt, weights = weight) %>% tidy())
avg_rank <- rbind(avg_rank, lm_robust(gender ~ 1, dt, weights = weight) %>% tidy())
avg_rank <- rbind(avg_rank, lm_robust(race_ethnicity ~ 1, dt, weights = weight) %>% tidy())
avg_rank$dt <- "Raw Data"
avg_rank <- avg_rank %>%
  rename(item = outcome) %>%
  select(item, estimate, conf.low, conf.high, dt)



# Visualization ==============================================================

avg_gg_comb <- rbind(avg_rank,
                     avg_rank.w,
                     avg_rank.i)

width_par <- 0.5

avg_gg_comb %>% 
  ggplot(aes(y = fct_reorder(item, -estimate, mean), 
             x = estimate, group = dt, color = dt)) +
  scale_fill_brewer(palette = "Accent") +
  geom_vline(xintercept = 2.5, lty = 2, color = alpha("black", 0.5), linewidth = 0.3) +  
  geom_point(aes(shape = dt), position = position_dodge(width = width_par), size = 1.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0,
                position = position_dodge(width_par), size = 0.6) +
  scale_color_manual(values = c("Direct" = "darkcyan", 
                                "IPW" = "maroon4",
                                "Raw Data" =  alpha("dimgray", 0.5))) +
  geom_text(aes(x = conf.high + 0.1,
                label = round(estimate, 1.5)),
            position = position_dodge(width = width_par),
            size = 1.5,
            color = "black") +
  # facet_grid(sample ~ .) +
  xlim(1, 4.1) +
  ylab("") +
  xlab("") +
  theme_classic(base_rect_size = 11/44) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        text = element_text(size = 8))


ggsave(here::here("fig", "weight-avg-rank-sample.pdf"), 
       width = 4*1.2, height = 4)






# # The following is an old code before using the package
# 
# 
# # Downsize data
# dt <- main %>%
#   mutate(id = 1:nrow(main)) %>%
#   rename(
#     party = app_identity_1,
#     religion = app_identity_2,
#     gender = app_identity_3,
#     race_ethnicity = app_identity_4
#   ) %>%
#   left_join(imp_w, by = "app_identity") %>%
#   select(
#     party, religion, gender, race_ethnicity, id,
#     ideo7, pid7, race, app_identity, bias_weight
#   ) %>%
#   filter(
#     !is.na(ideo7),
#     !is.na(pid7),
#     !is.na(race)
#   )
# 
# 
# # Visualize average ranks =============================================
# 
# avg_rank <- as.data.frame(NA)
# avg_rank <- lm_robust(party ~ 1, dt) %>% tidy()
# avg_rank <- rbind(avg_rank, lm_robust(religion ~ 1, dt) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(gender ~ 1, dt) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(race_ethnicity ~ 1, dt) %>% tidy())
# avg_rank$dt <- "Raw Data"
# 
# avg_rank.w <- as.data.frame(NA)
# avg_rank.w <- lm_robust(party ~ 1, dt, bias_weight) %>% tidy()
# avg_rank.w <- rbind(avg_rank.w, lm_robust(religion ~ 1, dt, bias_weight) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(gender ~ 1, dt, bias_weight) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(race_ethnicity ~ 1, dt, bias_weight) %>% tidy())
# avg_rank.w$dt <- "Bias Corrected"
# 
# avg_rank.w <- avg_rank.w %>%
#   mutate(conf.low = estimate - 1.96*std.error*1.5,
#          conf.high = estimate + 1.96*std.error*1.5)
# 
# 
# avg_gg_all <- rbind(avg_rank, avg_rank.w) %>%
#   mutate(sample = "All Samples")
# 
# 
# # Whites Only
# avg_rank <- as.data.frame(NA)
# avg_rank <- lm_robust(party ~ 1, dt, subset = race==1) %>% tidy()
# avg_rank <- rbind(avg_rank, lm_robust(religion ~ 1, dt, subset = race==1) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(gender ~ 1, dt, subset = race==1) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(race_ethnicity ~ 1, dt, subset = race==1) %>% tidy())
# avg_rank$dt <- "Raw Data"
# 
# avg_rank.w <- as.data.frame(NA)
# avg_rank.w <- lm_robust(party ~ 1, dt, bias_weight, subset = race==1) %>% tidy()
# avg_rank.w <- rbind(avg_rank.w, lm_robust(religion ~ 1, dt, bias_weight, subset = race==1) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(gender ~ 1, dt, bias_weight, subset = race==1) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(race_ethnicity ~ 1, dt, bias_weight, subset = race==1) %>% tidy())
# avg_rank.w$dt <- "Bias Corrected"
# 
# avg_rank.w <- avg_rank.w %>%
#   mutate(conf.low = estimate - 1.96*std.error*1.5,
#          conf.high = estimate + 1.96*std.error*1.5,)
# 
# avg_gg_white <- rbind(avg_rank, avg_rank.w) %>%
#   mutate(sample = "Whites (n=797)")
# 
# 
# # Blacks Only
# avg_rank <- as.data.frame(NA)
# avg_rank <- lm_robust(party ~ 1, dt, subset = race==2) %>% tidy()
# avg_rank <- rbind(avg_rank, lm_robust(religion ~ 1, dt, subset = race==2) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(gender ~ 1, dt, subset = race==2) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(race_ethnicity ~ 1, dt, subset = race==2) %>% tidy())
# avg_rank$dt <- "Raw Data"
# 
# avg_rank.w <- as.data.frame(NA)
# avg_rank.w <- lm_robust(party ~ 1, dt, bias_weight, subset = race==2) %>% tidy()
# avg_rank.w <- rbind(avg_rank.w, lm_robust(religion ~ 1, dt, bias_weight, subset = race==2) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(gender ~ 1, dt, bias_weight, subset = race==2) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(race_ethnicity ~ 1, dt, bias_weight, subset = race==2) %>% tidy())
# avg_rank.w$dt <- "Bias Corrected"
# 
# avg_rank.w <- avg_rank.w %>%
#   mutate(conf.low = estimate - 1.96*std.error*1.5,
#          conf.high = estimate + 1.96*std.error*1.5,)
# 
# avg_gg_black <- rbind(avg_rank, avg_rank.w) %>%
#   mutate(sample = "Blacks (n=116)")
# 
# 
# # Latinos Only
# avg_rank <- as.data.frame(NA)
# avg_rank <- lm_robust(party ~ 1, dt, subset = race==3) %>% tidy()
# avg_rank <- rbind(avg_rank, lm_robust(religion ~ 1, dt, subset = race==3) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(gender ~ 1, dt, subset = race==3) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(race_ethnicity ~ 1, dt, subset = race==3) %>% tidy())
# avg_rank$dt <- "Raw Data"
# 
# avg_rank.w <- as.data.frame(NA)
# avg_rank.w <- lm_robust(party ~ 1, dt, bias_weight, subset = race==3) %>% tidy()
# avg_rank.w <- rbind(avg_rank.w, lm_robust(religion ~ 1, dt, bias_weight, subset = race==3) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(gender ~ 1, dt, bias_weight, subset = race==3) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(race_ethnicity ~ 1, dt, bias_weight, subset = race==3) %>% tidy())
# avg_rank.w$dt <- "Bias Corrected"
# 
# avg_rank.w <- avg_rank.w %>%
#   mutate(conf.low = estimate - 1.96*std.error*1.5,
#          conf.high = estimate + 1.96*std.error*1.5)
# 
# avg_gg_latino <- rbind(avg_rank, avg_rank.w) %>%
#   mutate(sample = "Latinos (n=87)")
# 
# 
# # Asians Only
# avg_rank <- as.data.frame(NA)
# avg_rank <- lm_robust(party ~ 1, dt, subset = race==4) %>% tidy()
# avg_rank <- rbind(avg_rank, lm_robust(religion ~ 1, dt, subset = race==4) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(gender ~ 1, dt, subset = race==4) %>% tidy())
# avg_rank <- rbind(avg_rank, lm_robust(race_ethnicity ~ 1, dt, subset = race==4) %>% tidy())
# avg_rank$dt <- "Raw Data"
# 
# avg_rank.w <- as.data.frame(NA)
# avg_rank.w <- lm_robust(party ~ 1, dt, bias_weight, subset = race==4) %>% tidy()
# avg_rank.w <- rbind(avg_rank.w, lm_robust(religion ~ 1, dt, bias_weight, subset = race==4) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(gender ~ 1, dt, bias_weight, subset = race==4) %>% tidy())
# avg_rank.w <- rbind(avg_rank.w, lm_robust(race_ethnicity ~ 1, dt, bias_weight, subset = race==4) %>% tidy())
# avg_rank.w$dt <- "Bias Corrected"
# 
# avg_rank.w <- avg_rank.w %>%
#   mutate(conf.low = estimate - 1.96*std.error*1.5,
#          conf.high = estimate + 1.96*std.error*1.5)
# 
# avg_gg_asian <- rbind(avg_rank, avg_rank.w) %>%
#   mutate(sample = "Asians (n=30)")
# 
# 
# 
# avg_gg_comb <- rbind(avg_gg_all,
#                      avg_gg_white,
#                      avg_gg_black,
#                      avg_gg_latino,
#                      avg_gg_asian)
# 
# avg_gg_comb$sample <-  factor(avg_gg_comb$sample, 
#                               levels = c("All Samples", 
#                                          "Whites (n=797)", 
#                                          "Blacks (n=116)", 
#                                          "Latinos (n=87)",
#                                          "Asians (n=30)"))
# 
# 
# 
# avg_gg_comb %>% 
#   ggplot(aes(y = fct_reorder(outcome, -estimate, mean), 
#                       x = estimate, group = dt, color = dt)) +
#   geom_rect(aes(fill = sample),
#             xmin = -Inf,xmax = Inf,
#             ymin = -Inf,ymax = Inf, alpha = 0.01,
#             show_guide = FALSE
#   ) +    
#   scale_fill_brewer(palette = "Accent") +
#   geom_vline(xintercept = 2.5, lty = 2, color = alpha("darkred", 0.5), linewidth = 0.3) +  
#   geom_point(aes(shape = dt), position = position_dodge(width = 1.2), size = 1.5) +
#   geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0,
#                 position = position_dodge(1.2), size = 0.6) +
#   scale_color_manual(values = c("Bias Corrected" = "darkcyan", 
#                                 "Raw Data" =  alpha("dimgray", 0.5))) +
#   geom_text(aes(x = conf.high + 0.1,
#              label = round(estimate, 1.5)),
#             position = position_dodge(width = 1.2),
#             size = 1.5,
#             color = "black") +
#   facet_grid(sample ~ .) +
#   xlim(1, 4.1) +
#   ylab("") +
#   xlab("") +
#   theme_classic(base_rect_size = 11/44) +
#   theme(legend.position = "top",
#         legend.title = element_blank(),
#         legend.text = element_text(size = 6),
#         axis.text.y = element_text(size = 6),
#         text = element_text(size = 8))
# 
# ggsave(here::here("fig", "weight-avg-rank.pdf"), 
#        width = 4.5, height = 4.5)
# 

# 
# 
# # Figures for Talks
# avg_rank %>% ggplot(aes(y = fct_reorder(outcome, -estimate, mean), 
#                       x = estimate, group = dt, color = dt)) +
#   geom_vline(xintercept = 2.5, lty = 2, color = "gray") +  
#   geom_point(aes(shape = dt), position = position_dodge(width = 0.5), size = 2) +
#   geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0,
#                 position = position_dodge(0.5), size = 1.3) +
#   scale_color_manual(values = c("Bias Corrected" = "darkcyan", 
#                                 "Raw Data" =  "dimgray")) +
#   annotate("text", x = 1.9, y = 4.4, label = "raw data", color = "dimgray", size = 3) +      
#   xlim(1.5, 3.5) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# ggsave(here::here("fig", "weight-avg-rank_empty.pdf"), width = 0.9*5, height = 0.9*3)
# 
# 
# 
# avg_gg %>% ggplot(aes(y = fct_reorder(outcome, -estimate, mean), 
#                       x = estimate, group = dt, color = dt)) +
#   geom_vline(xintercept = 2.5, lty = 2, color = "gray") +  
#   geom_point(aes(shape = dt), position = position_dodge(width = 0.5), size = 2) +
#   geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0,
#                 position = position_dodge(0.5), size = 1.3) +
#   scale_color_manual(values = c("Bias Corrected" = "darkcyan", 
#                                 "Raw Data" =  "dimgray")) +
#   annotate("text", x = 1.7, y = 3.6, label = "bias corrected", color = "darkcyan", size = 3) +    
#   annotate("text", x = 1.9, y = 4.4, label = "raw data", color = "dimgray", size = 3) +      
#   xlim(1.5, 3.5) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# ggsave(here::here("fig", "weight-avg-rank_notext.pdf"), width = 0.9*5, height = 0.9*3)
# 
# 
# 
# 
# avg_rank.w %>% ggplot(aes(y = fct_reorder(outcome, -estimate, mean), 
#                       x = estimate, group = dt, color = dt)) +
#   geom_vline(xintercept = 2.5, lty = 2, color = "gray") +  
#   geom_point(aes(shape = dt), size = 3) +
#   geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0,
#                 size = 1.5) +
#   scale_color_manual(values = c("Bias Corrected" = "darkcyan", 
#                                 "Raw Data" =  "dimgray")) +
#   annotate("text", x = 2.3, y = 4.3, label = "More important", 
#            size = 3, color = "gray") +
#   geom_segment(aes(x = 1.8, y = 4.3, xend = 1.5, yend = 4.3),
#                arrow = arrow(length = unit(0.2, "cm")), color = "gray") +  
#   xlim(1.5, 3.5) +
#   ylab("") +
#   xlab("") +
#   theme_bw() +
#   theme(legend.position = "none")
# 
# ggsave(here::here("fig", "weight-avg-rank-talk.pdf"), width = 5, height = 3)
# 
# 
# 
# # Correlate with partisanship =============================================
# 
# dt <- dt %>%
#   mutate(pid7_sq = pid7^2) %>%
#   filter(pid7 != 8)
# 
# plot(jitter(dt$party) ~ jitter(dt$pid7))
# abline(lm(party ~ pid7, dt), col = "darkred")
# 
# summary(lm(party ~ pid7, dt))
# summary(lm(party ~ pid7 + pid7_sq, dt))
# 
# summary(lm(party ~ ideo7, dt))


# target <- "party"
# others <- c("religion", "gender", "race_ethnicity")
# 
# ## Raw data
# viz_ranking(
#   dt,
#   target_item = target,
#   other_items = others
# ) -> p
# 
# 
# ## With bias correction
# viz_ranking(
#   dt,
#   target_item = target,
#   other_items = others,
#   weight = dt$bias_weight
# ) -> p_w
# 
# ### Check
# ggarrange(
#   p + ggtitle("Raw Data"),
#   p_w + ggtitle("With Bias Correction"),
#   ncol = 2
# )
# 
# ggsave(p_w, here::here("fig", "weight_descriptive.pdf"), width = 7, height = 4)
# 
# 
# ## Results by Race
# dt_white <- dt %>% filter(race == 1)
# dt_black <- dt %>% filter(race == 2)
# dt_latino <- dt %>% filter(race == 3)
# dt_asian <- dt %>% filter(race == 4)
# 
# 
# p_white <- viz_ranking(
#   dt_white,
#   target_item = target,
#   other_items = others, weight = dt_white$bias_weight
# ) +
#   ggtitle("Whites")
# 
# p_black <- viz_ranking(
#   dt_black,
#   target_item = target,
#   other_items = others, weight = dt_black$bias_weight
# ) +
#   ggtitle("Blacks")
# 
# p_latino <- viz_ranking(
#   dt_latino,
#   target_item = target,
#   other_items = others, weight = dt_latino$bias_weight
# ) +
#   ggtitle("Latinos")
# 
# p_asian <- viz_ranking(
#   dt_asian,
#   target_item = target,
#   other_items = others, weight = dt_asian$bias_weight
# ) +
#   ggtitle("Asians")
# 
# ### Check
# p_com <- ggarrange(p_white, p_black, p_latino, p_asian)
# ggsave(
#   here::here("fig", "weight_descriptive_race.pdf"),
#   p_com,
#   width = 12, height = 7
# )
