source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

library(questionr) # for wtd.table()
library(RColorBrewer)  

# Grab rankings and weights
imp_w <- read_csv(here::here("data/tidy", "temp_weight.csv")) %>%
  mutate(
    app_identity = as.character(ranking),
    bias_weight = weight
  ) %>%
  dplyr::select(app_identity, bias_weight)

# Grab main data
main <- df_list$main


# Data processing ==============================================================
main <- main %>%
  mutate(
    across(where(is.labelled), ~ as.numeric(as.character(.)))
  )

# Reference set: (party, religion, gender, race)

# Downsize data
dt <- main %>%
  mutate(id = 1:nrow(main)) %>%
  rename(
    party = app_identity_1,
    religion = app_identity_2,
    gender = app_identity_3,
    race_ethnicity = app_identity_4
  ) %>%
  left_join(imp_w, by = "app_identity") %>%
  select(
    party, religion, gender, race_ethnicity, id,
    ideo7, pid7, race, app_identity, bias_weight
  ) %>%
  filter(
    !is.na(ideo7),
    !is.na(pid7),
    !is.na(race)
  )

# Visualize full PMF============================================================

# Bias Corrected PMF
perm <- combinat::permn(x = c("Party","Religion","Gender","Race")) 
pmf <- wtd.table(x = dt$app_identity,
                 weights = dt$bias_weight/dim(dt)[1]) %>%
  data.frame() %>%
  mutate(party = substr(Var1, 1, 1),
         religion = substr(Var1, 2, 2),
         gender = substr(Var1, 3, 3),
         race = substr(Var1, 4, 4),
         type = case_when(party == 1 ~ "A",
                          religion == 1 ~ "B",
                          gender == 1 ~ "C",
                          race == 1 ~ "D"),
         ordering = case_when(Var1 == 1234 ~ "Party-Religion-Gender-Race",
                              Var1 == 1243 ~ "Party-Religion-Race-Gender",
                              Var1 == 1324 ~ "Party-Gender-Religion-Race",
                              Var1 == 1342 ~ "Party-Race-Religion-Gender",
                              Var1 == 1423 ~ "Party-Gender-Race-Religion",
                              Var1 == 1432 ~ "Party-Race-Gender-Religion",
                              Var1 == 2134 ~ "Religion-Party-Gender-Race",
                              Var1 == 2143 ~ "Religion-Party-Race-Gender",
                              Var1 == 2314 ~ "Gender-Party-Religion-Race",
                              Var1 == 2341 ~ "Race-Party-Religion-Gender",
                              Var1 == 2413 ~ "Gender-Party-Race-Religion",
                              Var1 == 2431 ~ "Race-Party-Gender-Religion",
                              Var1 == 3124 ~ "Religion-Gender-Party-Race",
                              Var1 == 3142 ~ "Religion-Race-Party-Gender",
                              Var1 == 3214 ~ "Gender-Religion-Party-Race",
                              Var1 == 3241 ~ "Race-Religion-Party-Gender",
                              Var1 == 3412 ~ "Gender-Race-Party-Religion",
                              Var1 == 3421 ~ "Race-Gender-Party-Religion",
                              Var1 == 4123 ~ "Religion-Gender-Race-Party",
                              Var1 == 4132 ~ "Religion-Race-Gender-Party",
                              Var1 == 4213 ~ "Gender-Religion-Race-Party",
                              Var1 == 4231 ~ "Race-Religion-Gender-Party",
                              Var1 == 4312 ~ "Gender-Race-Religion-Party",
                              Var1 == 4321 ~ "Race-Gender-Religion-Party"),
         ordering = tolower(ordering),
         data = "Bias Corrected") %>%
  arrange(type, Var1)


p <- ggplot(pmf, aes(x = ordering, y = Freq, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = brewer.pal(n = 4, name = 'Set2')) +
  annotate("rect",
           xmin=7, xmax=12,
           ymin=-Inf, ymax=Inf, alpha=0.1, fill="black") +  
  ylab("Proportion of Unique Ranking Profiles") +
  xlab("") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

p


# Raw Data
pmf_raw <- wtd.table(x = dt$app_identity) %>%
  data.frame() %>%
  mutate(party = substr(Var1, 1, 1),
         religion = substr(Var1, 2, 2),
         gender = substr(Var1, 3, 3),
         race = substr(Var1, 4, 4),
         type = case_when(party == 1 ~ "A",
                          religion == 1 ~ "B",
                          gender == 1 ~ "C",
                          race == 1 ~ "D"),
         Freq = Freq / dim(dt)[1], # Make it proportional
         ordering = case_when(Var1 == 1234 ~ "Party-Religion-Gender-Race",
                              Var1 == 1243 ~ "Party-Religion-Race-Gender",
                              Var1 == 1324 ~ "Party-Gender-Religion-Race",
                              Var1 == 1342 ~ "Party-Race-Religion-Gender",
                              Var1 == 1423 ~ "Party-Gender-Race-Religion",
                              Var1 == 1432 ~ "Party-Race-Gender-Religion",
                              Var1 == 2134 ~ "Religion-Party-Gender-Race",
                              Var1 == 2143 ~ "Religion-Party-Race-Gender",
                              Var1 == 2314 ~ "Gender-Party-Religion-Race",
                              Var1 == 2341 ~ "Race-Party-Religion-Gender",
                              Var1 == 2413 ~ "Gender-Party-Race-Religion",
                              Var1 == 2431 ~ "Race-Party-Gender-Religion",
                              Var1 == 3124 ~ "Religion-Gender-Party-Race",
                              Var1 == 3142 ~ "Religion-Race-Party-Gender",
                              Var1 == 3214 ~ "Gender-Religion-Party-Race",
                              Var1 == 3241 ~ "Race-Religion-Party-Gender",
                              Var1 == 3412 ~ "Gender-Race-Party-Religion",
                              Var1 == 3421 ~ "Race-Gender-Party-Religion",
                              Var1 == 4123 ~ "Religion-Gender-Race-Party",
                              Var1 == 4132 ~ "Religion-Race-Gender-Party",
                              Var1 == 4213 ~ "Gender-Religion-Race-Party",
                              Var1 == 4231 ~ "Race-Religion-Gender-Party",
                              Var1 == 4312 ~ "Gender-Race-Religion-Party",
                              Var1 == 4321 ~ "Race-Gender-Religion-Party"),
         ordering = tolower(ordering),
         data = "Raw Data") %>%
  arrange(type, Var1)

pmf_com <- rbind(pmf, pmf_raw)


p2 <- ggplot(pmf_com, aes(x = ordering, y = Freq, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = brewer.pal(n = 4, name = 'Set2')) +
  annotate("rect",
           xmin=7, xmax=12,
           ymin=-Inf, ymax=Inf, alpha=0.1, fill="black") +  
  ylab("Proportion of Unique Ranking Profiles") +
  xlab("") +
  facet_grid(~ data) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()

p2


p
ggsave(here::here("fig", "weight-PMF.pdf"),
        width = 5, height = 4)


p2
ggsave(here::here("fig", "weight-PMF-combined.pdf"),
       width = 8, height = 4)




# Visualize descriptive statistics =============================================

avg_rank <- as.data.frame(NA)
avg_rank <- lm_robust(party ~ 1, dt) %>% tidy()
avg_rank <- rbind(avg_rank, lm_robust(religion ~ 1, dt) %>% tidy())
avg_rank <- rbind(avg_rank, lm_robust(gender ~ 1, dt) %>% tidy())
avg_rank <- rbind(avg_rank, lm_robust(race ~ 1, dt) %>% tidy())
avg_rank$dt <- "Raw Data"

avg_rank.w <- as.data.frame(NA)
avg_rank.w <- lm_robust(party ~ 1, dt, bias_weight) %>% tidy()
avg_rank.w <- rbind(avg_rank.w, lm_robust(religion ~ 1, dt, bias_weight) %>% tidy())
avg_rank.w <- rbind(avg_rank.w, lm_robust(gender ~ 1, dt, bias_weight) %>% tidy())
avg_rank.w <- rbind(avg_rank.w, lm_robust(race ~ 1, dt, bias_weight) %>% tidy())
avg_rank.w$dt <- "Bias Corrected"


avg_gg <- rbind(avg_rank, avg_rank.w)

avg_gg %>% ggplot(aes(y = outcome, x = estimate, group = dt)) +
  geom_point(aes(shape = dt), position = position_dodge(width = 0.4)) +
  xlim(0, 4) +
  ylab("") +
  xlab("") +
  theme_bw()



target <- "party"
others <- c("religion", "gender", "race_ethnicity")

## Raw data
viz_ranking(
  dt,
  target_item = target,
  other_items = others
) -> p


## With bias correction
viz_ranking(
  dt,
  target_item = target,
  other_items = others,
  weight = dt$bias_weight
) -> p_w

### Check
ggarrange(
  p + ggtitle("Raw Data"),
  p_w + ggtitle("With Bias Correction"),
  ncol = 2
)

ggsave(p_w, here::here("fig", "weight_descriptive.pdf"), width = 7, height = 4)


## Results by Race
dt_white <- dt %>% filter(race == 1)
dt_black <- dt %>% filter(race == 2)
dt_latino <- dt %>% filter(race == 3)
dt_asian <- dt %>% filter(race == 4)


p_white <- viz_ranking(
  dt_white,
  target_item = target,
  other_items = others, weight = dt_white$bias_weight
) +
  ggtitle("Whites")

p_black <- viz_ranking(
  dt_black,
  target_item = target,
  other_items = others, weight = dt_black$bias_weight
) +
  ggtitle("Blacks")

p_latino <- viz_ranking(
  dt_latino,
  target_item = target,
  other_items = others, weight = dt_latino$bias_weight
) +
  ggtitle("Latinos")

p_asian <- viz_ranking(
  dt_asian,
  target_item = target,
  other_items = others, weight = dt_asian$bias_weight
) +
  ggtitle("Asians")

### Check
p_com <- ggarrange(p_white, p_black, p_latino, p_asian)
ggsave(
  here::here("fig", "weight_descriptive_race.pdf"),
  p_com,
  width = 12, height = 7
)
