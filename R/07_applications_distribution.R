source(here::here("R", "utilities.R"))
load(here("data", "tidy", "df_list.Rda"))

# install.packages("devtools", dependencies = T)
# devtools::install_github("sysilviakim/ranking")

library(questionr) # for wtd.table()
library(RColorBrewer)  

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

# Inverse probability weighting
w <- imprr_weights(
  data = data,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity",
  weight = data$weight
)

w0 <- imprr_weights(
  data = data,
  J = 4,
  main_q = "app_identity",
  anchor_q =  "anc_identity",
  anc_correct = "anc_correct_identity"
)

print(w)
print(w0)

sum(w$weights$w)
sum(w0$weights$w)

w$weights <- w$weights %>%
  rename(alt_w = w)

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
  left_join(w0$weights, by = "ranking") %>%
  mutate(w_multiplied = weight * w) %>%
  select(
    party, religion, gender, race_ethnicity, race, app_identity,
    ranking, weight, w, alt_w, w_multiplied) 

  

  

# Visualize full PMF============================================================

# Bias Corrected PMF
perm <- combinat::permn(x = c("Party","Religion","Gender","Race")) 
pmf <- wtd.table(x = dt$app_identity,
                 weights = dt$w_multiplied) %>%
  data.frame() %>%
  mutate(party = substr(Var1, 1, 1),
         religion = substr(Var1, 2, 2),
         gender = substr(Var1, 3, 3),
         race = substr(Var1, 4, 4),
         type = case_when(party == 1 ~ "A",
                          religion == 1 ~ "B",
                          gender == 1 ~ "C",
                          race == 1 ~ "D"),
         Freq = Freq / sum(dt$w_multiplied), # Make it proportional         
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
ggsave(here::here("fig", "weight-PMF-survey-weights.pdf"),
        width = 5, height = 4)


p2
ggsave(here::here("fig", "weight-PMF-combined-survey-weights.pdf"),
       width = 8, height = 4)



# Figures for Talks
  pmf_com %>%
  mutate(type = ifelse(data == "Raw Data", type, "White")) %>%
  ggplot(aes(x = ordering, y = Freq, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(brewer.pal(n = 4, name = 'Set2'), 
                               alpha("white", 0))) +
  annotate("rect",
           xmin=7, xmax=12,
           ymin=-Inf, ymax=Inf, alpha=0.1, fill="black") +  
  ylab("Proportion of Unique Ranking Profiles") +
  xlab("") +
  facet_grid(~ data) +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip()  

  ggsave(here::here("fig", "weight-PMF-combined_empty.pdf"),
         width = 8, height = 4)
  
