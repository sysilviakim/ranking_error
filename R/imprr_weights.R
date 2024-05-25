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
#'

# source(here::here("R", "utilities.R"))
# load(here("data", "tidy", "df_list.Rda"))
#
# data <- df_list$main %>%
#   select(app_identity_1, app_identity_2, app_identity_3, app_identity_4,
#          anc_identity_1, anc_identity_2, anc_identity_3, anc_identity_4,
#          anc_correct_identity)
#
# main_q <- "app_identity"
# anchor_q <- "anc_identity"
# anc_correct <- "anc_correct_identity"
# J <- 4
# n_bootstrap = 10
# seed = 123456

imprr_weights <- function(data,
                          J = NULL,
                          main_q,
                          anchor_q,
                          anc_correct,
                          anc_correct_pattern = NULL,
                          n_bootstrap = 200,
                          seed = 123456) {
  # Setup ======================================================================
  N <- nrow(data)
  if (is.null(J)) {
    J <- nchar(data[[main_q]][[1]])
  }

  # # Check the validity of the input arguments ==================================
  # if (!(main_q %in% names(data))) {
  #   stop("The main question is not a valid column name in the given dataset.")
  # }
  # if (!(anchor_q %in% names(data))) {
  #   stop("The anchor question is not a valid column name in the given dataset.")
  # }
  # if (!(anc_correct %in% names(data))) {
  #   stop(
  #     paste0(
  #       "The indicator for passing the anchor question ",
  #       "is not a valid column name in the given dataset."
  #     )
  #   )
  # }
  # if (!all(main_labels %in% names(data))) {
  #   stop(
  #     paste0(
  #       "One or more of labels in the ranking options is not a valid column",
  #       " in the given dataaset."
  #     )
  #   )
  # }
  # if (length(main_labels) != J) {
  #   stop("Length of the label vector is not equal to the J.")
  # }
  # if (is.null(anc_correct_pattern)) {
  #   anc_correct_pattern <- seq(J) %>% paste(collapse = "")
  # }
  # if (nchar(anc_correct_pattern) != J) {
  #   stop("Length of anc_correct_pattern does not match J.")
  # }


  ## Anchor ranking only
  glo_anc <- data %>%
    select(matches(anchor_q)) %>%
    select(matches("_[[:digit:]]$"))

  ## Main ranking only (Silvia, I edited here slightly)
  glo_app <- data %>%
    select(matches(main_q)) %>%
    select(matches("_[[:digit:]]$"))

  # Step 1: Get the proportion of random answers
  p_non_random <- (mean(data[[anc_correct]]) - 1 / factorial(J)) /
    (1 - 1 / factorial(J))

  # Step 2: Get the uniform distribution
  U <- rep(1 / factorial(J), factorial(J))

  # Step 3: Get the observed PMF based on raw data
  ## Get raw counts of ranking profiles
  D_PMF_0 <- glo_app %>%
    unite(ranking, sep = "") %>%
    group_by(ranking) %>%
    count()

  ## Create sample space to merge
  perm_j <- permn(1:J)
  perm_j <- do.call(rbind.data.frame, perm_j)
  colnames(perm_j) <- c(paste0("position_", 1:J))
  perm_j <- perm_j %>%
    unite(col = "ranking", sep = "") %>%
    arrange(ranking)

  ## We need this because some rankings may not appear in the data
  PMF_raw <- perm_j %>%
    left_join(D_PMF_0, by = "ranking") %>%
    mutate(
      n = ifelse(is.na(n) == T, 0, n),
      prop = n / sum(n),
      prop = ifelse(is.na(prop), 0, prop)
    ) %>%
    arrange(ranking)


  # Step 4: Get the bias-corrected PMF
  ## Apply Equation A.11
  imp_PMF_0 <- (PMF_raw$prop - (U * (1 - p_non_random))) / p_non_random

  ## Recombine with ranking ID
  imp_PMF_1 <- perm_j %>%
    mutate(n = imp_PMF_0)

  # Step 5: Re-normalize the PMF
  ## The previous step may produce outside-the-bound values (negative proportions)
  imp_PMF <- imp_PMF_1 %>%
    mutate(
      n_adj = ifelse(n < 0, 0, n),
      n_renormalized = n_adj / sum(n_adj)
    ) %>%
    rename(
      prop = n,
      prop_adj = n_adj,
      prop_renormalized = n_renormalized
    ) %>%
    arrange(ranking)

  # Step 6: Get the bias-correction weight vector
  df_w <- perm_j %>%
    mutate(
      w = imp_PMF$prop_renormalized / PMF_raw$prop, # Inverse probability weight
      w = ifelse(w == Inf, 0, w),
      w = ifelse(is.na(w), 0, w)
    ) %>% # NA arise from 0/0
    arrange(ranking)



  # Summarize results

  return(list(
    est_p_random = 1 - p_non_random,
    obs_pmf = PMF_raw,
    corrected_pmf = imp_PMF,
    weights = df_w
  ))
}
