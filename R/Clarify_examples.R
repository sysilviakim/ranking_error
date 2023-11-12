# Clarify.R

library(tidyverse)
library(readxl)
library(clarify) # for clarify
library(nnet) # for multinomial logit

dt <- read_excel("McCauley and Posner, BJPS.xlsx")

head(dt)

colnames(dt)

# =============================================================================#
# Clarify: Making Predictions Using Statistical Results
# =============================================================================#

# Create three new variables
dt <- dt %>%
  mutate(treat = ifelse(Country == "Cote d'Ivoire", 1, 0),
         years = as.numeric(Yearsatsite),
         Education = as.numeric(Education))


# Run a Normal-identity model
par(mfrow=c(1, 1)) # Row, Column
hist(dt$Pray)

m4 <- lm(Pray ~ years + treat, data = dt)
summary(m4)


# Make predictions
set.seed(123)
sim_coefs <- sim(m4)

sim_est <- sim_setx(sim_coefs, 
                    x = list(years = 0:80, 
                             treat = 0:1))

plot(sim_est)


# Run a Bernoulli-logistic model
par(mfrow=c(1, 1)) # Row, Column
hist(dt$ReligID)

m5 <- glm(ReligID ~ treat + Education, data = dt, 
          family = binomial(link = "logit"))
summary(m5)


set.seed(123)
sim_coefs <- sim(m5)
sim_est <- sim_setx(sim_coefs, 
                    x = list(Education = 1:18, 
                             treat = 0:1))

plot(sim_est) +
  ylab("Predicted Probabilities") +
  ggtitle("Religion as the Most Important Identity")

ggsave(here::here("fig", "Discrete_logit.pdf"),
       width = 5, height = 4)



# Run a Multinomial-logistic model
dt <- dt %>%
  mutate(Education = as.numeric(Education))

m6 <- multinom(PrimID ~ treat + Education, data = dt)
summary(m6)

set.seed(123)
sim_coefs <- sim(m6)

religion <- sim_setx(sim_coefs, x = list(Education = 0:18, treat = 0:1),
                     outcome = "Religion")
gender <- sim_setx(sim_coefs, x = list(Education = 0:18, treat = 0:1),
                   outcome = "Gender")
national <- sim_setx(sim_coefs, x = list(Education = 0:18, treat = 0:1),
                     outcome = "National")
class <- sim_setx(sim_coefs, x = list(Education = 0:18, treat = 0:1),
                  outcome = "Occupation/Class")
ethnic <- sim_setx(sim_coefs, x = list(Education = 0:18, treat = 0:1),
                   outcome = "Ethnic")
other <- sim_setx(sim_coefs, x = list(Education = 0:18, treat = 0:1),
                  outcome = "Other")


plot(religion) + ggtitle("Religion")  -> p1
plot(gender) + ggtitle("Gender")      -> p2
plot(national) + ggtitle("National")  -> p3
plot(class) + ggtitle("Class")        -> p4
plot(ethnic) + ggtitle("Ethnic")      -> p5
plot(other) + ggtitle("Other")        -> p6


ggpubr::ggarrange(p1, p2, p3, p4, p5, p6,
                  ncol = 3, nrow = 2, common.legend = T)


ggsave(here::here("fig", "Discrete_multi.pdf"),
       width = 6, height = 5)


