##################################################################################
# sim.R
# Created: 10/22/2022
# Aim: to simulate the effects of pattern ranking with various true distributions
##################################################################################

library(combinat) # For permn
library(gtools) # For permute
library(tidyverse)
library(styler)
library(PLMIX) # For switch_ord_rank()
rm(list=ls())

source(here::here("R/rpluce.R")) # READING THE FUNCTION THAT DRAW FROM PRACKETT-LUCE
##################################################################
# Simulation Parameters
##################################################################

N = 2000 # Number of units
J = 3  # Number of items


##################################################################
# (1) Generate the Sample Space for Simulation 
##################################################################

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Generate the population ranking distribution
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
set.seed(102)
prob_vec <- rep(1/J, J) # Uniform first-choice probability (can be MODIFIED)
prob_vec <- c(0.8,0.1,0.1) # A dominates B and C in the first-choice probabiltiy

print(prob_vec)

true_pref <- rpluce(N=N, t=J, prob=prob_vec) %>%
             as_tibble() 

head(true_pref) # Check

# Check the uniformity
ncol <- dim(true_pref)[2]
check <- true_pref %>% unite(order,sep="") 
table(check) 

# Getting the true ranking for each unit
true_rank <- PLMIX::rank_ord_switch(data=true_pref, 
                                   format_input="ordering") %>%
            as_tibble()

true_rank # Item_1 = A, Item _2 = B, Item_3 = C
# Note: if (3,2,1) this means A corresponds 3, B 2, and C 1. We use this later.


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Generate the ordered choice set in survey question
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

randomize <- rep(1/J,J) # Assumption 1: Order randomization
choice <- rpluce(N=N, t=J, prob=randomize) %>%
          as_tibble() 
head(choice)

# Check the uniformity 
check2 <- choice %>% unite(order,sep="")
table(check2) 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Generate respondents' stated ranked preferences
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

# (1) 100% attentive respondents

# Storage for observed patterns
obs_pattern <- data.frame(pattern1 = integer(),
                          pattern2 = integer(),
                          pattern3 = integer())

#----------------------------------------------------------------#
for(i in 1:dim(true_rank)[1]){ # For each i-th unit in true_rank

# Cast numbers to alphabets  
vec_pref <- true_rank[i,] %>% reshape2::melt() %>%
            mutate(variable = case_when(variable=="Item_1" ~ "a",
                                        variable=="Item_2" ~ "b",
                                        variable=="Item_3" ~ "c"))
vec_pref # Check

# Alphabet unit i sees in each position
position1 <- choice[i,1] %>% pull()
position2 <- choice[i,2] %>% pull()
position3 <- choice[i,3] %>% pull()

# Assign a value (rank) for each position given the alphabet unit i sees there
pattern1 <- vec_pref$value[vec_pref$variable==position1]
pattern2 <- vec_pref$value[vec_pref$variable==position2]
pattern3 <- vec_pref$value[vec_pref$variable==position3]

# Combine the result
comb <- data.frame(pattern1 = pattern1,
                   pattern2 = pattern2,
                   pattern3 = pattern3) 

# Stack in the storage
obs_pattern <- rbind(obs_pattern,comb) 

} # End of i loop
#----------------------------------------------------------------#

obs_pattern <- obs_pattern %>% unite(obs_rank, sep="")
table(obs_pattern)


# (2) 0% attentive respondents
# Note: These units only rank according to patterns, not based on preference
# Assuming uniform random patterns
perm <- combinat::permn(x=1:J)  # Get all elements in the sample space
perm <- as.data.frame(do.call(cbind, perm)) %>% 
        t() %>% 
        as_tibble() %>% 
        unite(rank, sep="") %>%
        pull() %>%
        sort()
perm

n_perm <- length(perm)           # Size of the permutation space
prob_pt <- rep(1/n_perm, n_perm) # Uniform patterns (can be MODIFIED later)
prob_pt <- c(0.05,0.3,0.15,0.15,0.3,0.05) # Zig-zag orientation

obs_random <- sample(x=perm,size=N, replace=T, prob=prob_pt)
obs_random <- data.frame(obs_random)

table(obs_random)

##################################################################
# (2) Compare the observed patterns
##################################################################

pdf(here::here("fig/ObsRanking.pdf"), width=8, height=6)
par(mfrow=c(2,2), mar=c(2.5,2.5,3,2), oma=c(0,4,2,0))
barplot(table(check)/N, 
        main="A. 100% Attentive (True pref: A >>> B = C)", 
        col="deepskyblue3", border=F)
mtext("Fixed Order (a,b,c)", 
      side=2, line=4, cex=1.2, font=2, col=rgb(0.1,0.3,0.5,0.5))
barplot(table(obs_random)/N, 
        main="B. 0% Attentive (Zig-Zag Orientation)", 
        col="deepskyblue3", border=F)
barplot(table(obs_pattern)/N, 
        main="C. 100% Attentive (True pref: A >>> B = C)", 
        col="deepskyblue3", border=F)
mtext("Item Order Randomization", 
      side=2, line=4, cex=1.2, font=2, col=rgb(0.1,0.3,0.5,0.5))
barplot(table(obs_random)/N, 
        main="D. 0% Attentive (Zig-Zag Orientation)", 
        col="deepskyblue3", border=F)
title("*Observed* Ranking Patterns",outer=T, col.main="deepskyblue4")

dev.off()

##################################################################
# (2) Estimate the Quantities of Interest
##################################################################

# Transforming observed rankings into "true" orderings
# Create ID for join
obs_pattern$id <- as.numeric(row.names(obs_pattern))
obs_random$id <- as.numeric(row.names(obs_pattern))
choice$id <- as.numeric(row.names(choice))

# Check
head(obs_pattern)
head(obs_random)
head(choice)


obs_pref1 <- choice %>% left_join(obs_pattern) %>%                      # Join two datasets
             gather("V1", "V2", "V3", key="position", value="item") %>% # Turn into a long format
             mutate(rank = case_when(position=="V1" ~ str_sub(obs_rank, 1,1),      # First value
                                     position=="V2" ~ str_sub(obs_rank, 2,2),      # Second value
                                     position=="V3" ~ str_sub(obs_rank, 3,3))) %>% # Third value
             arrange(id, rank)


obs_pref2 <- choice %>% left_join(obs_random) %>%                      # Join two datasets
             gather("V1", "V2", "V3", key="position", value="item") %>% # Turn into a long format
             mutate(rank = case_when(position=="V1" ~ str_sub(obs_random, 1,1),      # First value
                                     position=="V2" ~ str_sub(obs_random, 2,2),      # Second value
                                     position=="V3" ~ str_sub(obs_random, 3,3))) %>% # Third value
             arrange(id, rank)

# Check the "true" ranking data
head(obs_pref1)
head(obs_pref2)


# 2.1: Distribution of unique rankings (the most cease summary)
# 100% Attentive Respondents
freq_pref1 <-  obs_pref1[,c(1,4,5)] %>% 
               spread(key="rank", value="item") %>%
               dplyr::select(-id) %>%
               unite("true_order", sep="") 

# 0% Attentive Respondents
freq_pref2 <-  obs_pref2[,c(1,4,5)] %>% 
               spread(key="rank", value="item") %>%
               dplyr::select(-id) %>%
               unite("true_order", sep="") 


# Visualizing What Happens When We Extract "True" Orderings from Data with Item Order Randomization
pdf(here::here("fig/QOI.pdf"), width=8, height=6)
par(mfrow=c(1,2), mar=c(2.5,2.5,3,2), oma=c(0,4,2,0))
barplot(table(freq_pref1)/N, 
        main="A. 100% Attentive (True pref: A >>> B = C)", 
        col="deepskyblue3", border=F)
mtext("Item Order Randomization", 
      side=2, line=4, cex=1.2, font=2, col=rgb(0.1,0.3,0.5,0.5))
barplot(table(freq_pref2)/N, 
        main="B. 0% Attentive (Zig-Zag Orientation)", 
        col="deepskyblue3", border=F)

dev.off()




