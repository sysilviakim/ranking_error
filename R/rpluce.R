#############################################################
# WRITE A FUNCTION TO DRAW SAMPLES FROM PLACKET-LUCE MODLE
#############################################################

#' @title rpluce
#'
#' @description  \code{rpluce} draws samples from Placket-Luce model
#' @param N total number of samples to draw
#' @param t total number of items (currently t<27)
#' @param prob a vector of choice probabilities
#'
#' @return A data frame consisting of drawn samples
#' @examples
#' SimRank <- rpluce(N = 5000, t = 4, prob = c(0.5, 0.3, 0.2, 0.1))
#' head(SimRank)
#' @export

# THE FOLLOWING ALGORITHMS FOLLOW Xia (2019, 20-22)
# --- "Learning and Decision-Making from Rank Data"
# ALGORITHM 2.1: Efficient Sampling from Plackett-Luce
# ALGORITHM 2.2: Efficient Sampling from RUM (Random Utility Model)

##########################################################################
# ALGORITHM 2.1: Efficient Sampling from Plackett-Luce
# This is based on the "gamma" parameterization of the PL model
##########################################################################

rpluce <- function(N, t, prob) {
  gamma.true <- prob
  # STORAGE OF RANKINGS with N-rows and t-columns
  R.set <- matrix(nrow = N, ncol = t, NA)
  item <- letters[1:t] # VECTOR OF ITEMS (currently, t<27)

  # LOOP OVER N ACCESSORS
  for (j in 1:N) {
    # INITIALIZATION +++++++++++++++++++++++++++++++++++#
    R <- character() # STORAGE OF RANKING
    A <- item # INITIAL CHOICE SET
    gamma <- gamma.true # INITIAL CHOICE PROBABILITY
    M <- length(item) - 1 # NUMBER OF LOOPS
    #+++++++++++++++++++++++++++++++++++++++++++++++++++#

    # LOOP WITHIN ONE ACCESSOR
    for (i in 1:M) {
      # DRAW FROM A MULTINOMIAL PMF
      draw <- rmultinom(n = 1, size = 1, prob = gamma)
      draw <- as.vector(t(draw))

      choice <- A[draw == 1] # ITEM THAT WAS DRAWN (draw==1)
      R <- cbind(R, choice) # CUMULATIVE ITEMS THAT WERE DRAWN SO FAR

      # RE-NORMALIZE THE REMAINING CHOICE SET
      A <- A[draw == 0] # REMOVING T-th CHOICE (DRAWN ABOVE)
      gamma <- gamma[draw == 0] # REMOVING T-th CHOICE's PROBABILITY
      gamma <- gamma / sum(gamma) # NORMALIZE THE CHOICE PROBABILITY
      if (is.na(gamma)) { # This is for when the remaining prob is all 0
        gamma <- c(1, rep(0, (length(gamma) - 1)))
      } else {}
    }

    R <- cbind(R, A)
    # colnames(R) <- c("choice1", "choice2", "choice3", "choice4")

    R.set[j, ] <- R
  }

  R.set <- as.data.frame(R.set)
  return(R.set) # RETURNING RANKINGS OF t ITEMS FOR N ASSESSORS
}

##########################################################################
# ALGORITHM 2.2: Efficient Sampling from RUM (Random Utility Model)
# This is based on the "theta" parameterization of the PL model
##########################################################################
