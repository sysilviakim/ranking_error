#' Bias-correct the Distribution of Ranking Permutations
#' Using a Paired Anchor Question and Produce De-biased Average Rankings
#'
#' @description
#'
#' @importFrom dplyr `%>%`
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr across
#' @importFrom dplyr summarise
#' @importFrom tibble tibble
#' @importFrom tidyselect everything
#' @importFrom tidyselect matches
#' @importFrom tidyr pivot_longer
#' @importFrom combinat permn
#' @importFrom assertthat assert_that
#' @importFrom estimatr lm_robust
#'
#' @param data The input dataset with ranking data.
#' @param main_q Column name for the main ranking question to be analyzed.
#' @param anchor_q Column name for the paired anchor question.
#' @param anc_correct Indicator for passing the anchor question.
#' @param anc_correct_pattern The correct pattern to pass the anchor question
#' filter, given the reference set. Defaults to NULL. 
#' If NULL, it will taken on a J-length string with a natural progression of 
#' numbers, such as "123", "1234", "1234567", and so on.
#' @param main_labels Labels for ranking options in the main question.
#' @param n_bootstrap Number of bootstraps. Defaults to 1,000.
#' @param asymptotics Whether the computations use asymptotics or the given
#' data. Defaults to FALSE.
#' @param seed Seed for \text{set.seed} for reproducibility.

#########################################
# For my coding
# data <- prep_list$identity$full
# main_q <- "app_identity"
# anchor_q <- "anc_identity"
# anc_correct <- "anc_correct_identity"
# anc_correct_pattern <- NULL
# main_labels <- prep_list$identity$labels
# asymptotics <- TRUE
# n_bootstrap <- 200
# seed <- 123456

# Apply the bias correction to the PMF
temp <- imprr(data = data,
              main_q = main_q,
              anchor_q,
              anc_correct,
              anc_correct_pattern = NULL,
              main_labels,
              n_bootstrap = 250,
              asymptotics = TRUE,     
              seed = 123456
              )

gdat <- temp %>%
  mutate(est = ifelse(est < 0, 0, est)) %>%
  data.frame()


p <- ggplot(gdat, aes(est, ranking, fill = imp)) +
  geom_col() +
  scale_fill_manual(
   values = c("A. Raw Data" = "#128ba0",
              "B. Bias Corrected"  = "#b0015a")) +  
  geom_vline(xintercept = 1/24, lty=2, color = "dimgray") +
  xlab("Proportion of Unique Ranking Profiles") +
  ylab("Reference choice set: {Party, Religion, Gender, Race/Ethnicity}") +
  facet_wrap(~ imp, ncol = 1) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none")

ggsave(here::here("fig", "corrected_PMF.pdf"),
       width = 8, height = 5)


imprr <- function(data,
                  main_q,
                  anchor_q,
                  anc_correct,
                  anc_correct_pattern = NULL,
                  main_labels,
                  n_bootstrap = 1000,
                  asymptotics = FALSE,
                  seed = 123456) {
  # Setup ======================================================================
  N <- nrow(data)
  J <- nchar(data[[main_q]][[1]])
  
  # Check the validity of the input arguments ==================================
  if (!(main_q %in% names(data))) {
    stop("The main question is not a valid column name in the given dataset.")
  }
  if (!(anchor_q %in% names(data))) {
    stop("The anchor question is not a valid column name in the given dataset.")
  }
  if (!(anc_correct %in% names(data))) {
    stop(
      paste0(
        "The indicator for passing the anchor question ",
        "is not a valid column name in the given dataset."
      )
    )
  }
  if (!all(main_labels %in% names(data))) {
    stop(
      paste0(
        "One or more of labels in the ranking options is not a valid column",
        " in the given dataaset."
      )
    )
  }
  if (length(main_labels) != J) {
    stop("Length of the label vector is not equal to the J.")
  }
  if (is.null(anc_correct_pattern)) {
    anc_correct_pattern <- seq(J) %>% paste(collapse = "")
  }
  if (nchar(anc_correct_pattern) != J) {
    stop("Length of anc_correct_pattern does not match J.")
  }
  
  ## List for bootstrapped results
  list_PMF <- list_prop <- vector("list", length = n_bootstrap)

  # Boostrapping ===============================================================
  ## Sample with replacement
  set.seed(seed)
  for (i in 1:n_bootstrap) {
    ## Sample indices
    index <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
    
    ## This is the bootstrapped data
    boostrap_dat <- data[index, ]
    
    ## Anchor ranking only
    loc_anc <- boostrap_dat %>% 
      select(matches(anchor_q)) %>%
      select(matches("_[[:digit:]]$"))
    
    ## Main ranking only
    loc_app <- boostrap_dat %>% 
      select(all_of(main_labels))
    
    ## Originally unknown quantity 1: proportion of random answers
    p_non_random <- (mean(boostrap_dat[[anc_correct]]) - 1 / factorial(J)) / 
      (1 - 1 / factorial(J))
    
    ## Originally unknown quantity 2: distribution of random answers
    ## Directly apply bias correction to our QOIs

      if (asymptotics == FALSE) {
      ## We will fully learn from data including noise via sampling variability
      
      ## Empirical PMF of rankings in anchor
      # A <- f_anchor$Freq
      
      ## Estimated proportion of non-random responses
      B <- p_non_random
      
      ## True PMF of rankings in anchor
      C <- data.frame(t(1:J))
      
      f_random_avg <- (A_avg - (B * C)) / (1 - B)
    } else {
      ## We use theory to get f_random_avg
      ## We know that the entire PMF will follow a uniform distribution
      ## f_random <- rep(1 / factorial(J), factorial(J))
      ## We then compute average ranks based on the asymptotic distribution
      
      ## This is true as N goes to infinity
      f_random_avg <- (1 + J) / 2 
      B <- p_non_random
    }
    
    ## Distribution of error-free rankings
    D_PMF_0 <- loc_app %>%
      unite(ranking, sep="") %>%
      group_by(ranking) %>%
      count()
    
    ## Create sample space to merge
    perm_j <- combinat::permn(1:J) 
    perm_j <- do.call(rbind.data.frame, perm_j) 
    colnames(perm_j) <- c(paste0("position_", 1:J)) 
    perm_j <- perm_j %>% unite(col = "ranking", sep="")
    
    ## We need this because some rankings do not appear in the data
    D_PMF <- perm_j %>%
    left_join(D_PMF_0, by="ranking") %>%
    mutate( n = n/ sum(n),
            n = ifelse(is.na(n), 0, n)) 
    
    ## Get Uniform Distribution
    U <- 1/factorial(J)
    
    imp_PMF_0 <- (D_PMF$n - ((1 - p_non_random) * U)) / p_non_random
  
    imp_PMF <- perm_j %>%
      mutate(n = imp_PMF_0)
      
    ## This method may produce outside-the-bound values; temporarily truncate
    list_prop[[i]] <- p_non_random
    list_PMF[[i]] <- imp_PMF
  }
  message("Bootstrapping finished.")
  
  out_prop <- D_PMF %>% # Uncorrected estimate
     mutate(est = n,
            low = n,
            up  = n,
            imp = "A. Raw Data") %>%
    select(-n)
  
  # Improved average ranks =====================================================
  out_PMF <- do.call(rbind.data.frame, list_PMF) %>%
    group_by(ranking) %>%
    summarise(
      "est" = mean(n),
      "low" = quantile(n, prob = 0.025),
      "up" = quantile(n, prob = 0.975)
    ) %>%
    ungroup() %>%
    mutate(imp = "B. Bias Corrected")
  
  combine <- rbind(out_PMF, out_prop)
  
  return(combine)
}

