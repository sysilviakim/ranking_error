imprr <- function(data, # all data
                  rank_q, # string for ranking names
                  target,
                  anc_correct, # string for correctness indicator
                  n_bootstrap = 250,
                  asymptotics = FALSE) {
  # Prepare
  N <- dim(data)[1] # Number of obs
  J <- length(rank_q)
  target_nmb <- which(rank_q %in% target) # -th position
  
  # Storages for bootstrapping
  list_prop <- list()
  list_avg <- list()
  
  # Sampling with replacement
  
  set.seed(123456)
  
  for (i in 1:n_bootstrap) {
    index <- sample(1:nrow(data), size = dim(data)[1], replace = TRUE) # RESAMPLE WITH REPLACEMENT
    bs.dat <- data[index, ] # BOOTSTRAPPED DATA
    
    
    # Anchor ranking only
    loc_anc <- bs.dat %>% select(contains(paste0("anc_")) &
                                   matches("[[:digit:]]"))
    
    # Main ranking only
    loc_app <- bs.dat %>% select(all_of(rank_q))
    
    
    # Equation (8) -- proportion of random answers
    Corr <- bs.dat[anc_correct] %>% pull()
    adjust <- mean(Corr) - 1 / factorial(J)
    normalizer <- (1 - 1 / factorial(J))
    p_non_random <- adjust / normalizer
    
    
    # Equation (9) -- distribution of random answers
    
    # Directly apply bias correction to our QOIs
    
    A_avg <- loc_anc %>%
      summarise(across(everything(), ~ mean(as.numeric(.x))))
    
    
    
    if (asymptotics == FALSE) {
      # We will fully learn from data
      ## including, noise via sampling variability
      
      # A <- f_anchor$Freq / N # -- empirical pmf of rankings in anchor
      B <- p_non_random # -- estimated proportion of non-random
      C <- data.frame(t(1:J)) # -- true pmf of rankings in anchor
      
      f_random_avg <- (A_avg - (B * C)) / (1 - B)
    } else {
      # We use theory to get f_random_avg
      ## We know that the entire PMF will follow a uniform distribution
      ## f_random <- rep(1/factorial(J), factorial(J))
      ## We then compute average ranks based on the asymptotic distribution
      
      f_random_avg <- (1 + J) / 2 # This is true as n --> infinity
    }
    
    
    # Redefine B
    B <- p_non_random # -- estimated proportion of non-random
    
    # Equation (10) -- distribution of error-free rankings
    
    D_avg <- loc_app %>%
      summarise(across(everything(), ~ mean(as.numeric(.x))))
    
    E <- f_random_avg # -- estimated pmf of errors in the anchor
    
    imp_avg <- (D_avg - ((1 - B) * E)) / B
    
    # This method produces outside-the-bound values
    # --> Yuki is correcting this temporarily
    
    bound <- function(x) {
      ifelse(x < 1, 1, ifelse(x > J, J, x))
    }
    
    imp_avg <- imp_avg %>%
      mutate(across(everything(), ~ bound(.x)))
    
    list_prop[[i]] <- p_non_random
    list_avg[[i]] <- imp_avg
  }
  
  
  out_prop <- do.call(rbind.data.frame, list_prop)
  
  # Improved average ranks
  out_avg <- do.call(rbind.data.frame, list_avg) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise(
      "est" = mean(value),
      "low" = quantile(value, prob = 0.025),
      "up" = quantile(value, prob = 0.975)
    ) %>%
    ungroup() %>%
    mutate(imp = "improved")
  
  
  # Raw average ranks
  raw_avg <- data %>%
    select(all_of(rank_q)) %>%
    pivot_longer(everything())
  
  
  raw_est <- list()
  
  for (j in 1:J) {
    dt_j <- raw_avg %>%
      filter(name == rank_q[j])
    
    raw_ols <- lm_robust(as.numeric(value) ~ 1, dt_j) %>%
      tidy() %>%
      mutate(
        item = rank_q[j],
        imp = "raw data"
      ) %>%
      select(
        est = estimate,
        low = conf.low,
        up = conf.high,
        name = item,
        imp
      )
    
    raw_est[[j]] <- raw_ols
  }
  
  
  raw_est <- do.call(rbind.data.frame, raw_est)
  raw_est
  
  
  # Combine both estimates
  out <- rbind(out_avg, raw_est) %>%
    arrange(name)
  
  
  return(out) # return the list
}

