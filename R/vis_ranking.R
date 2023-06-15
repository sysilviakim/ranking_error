#' Visualize ATEs in Ranking Data
#'
#' @description \code{vis_ranking} visualizes all ATEs
#' in a typical ranking dataset.
#'
#' @importFrom generics tidy
#' @importFrom dplyr mutate
#' @importFrom dplyr `%>%`
#' @importFrom purrr map
#' 
#' @importFrom estimatr lm_robust
#' @importFrom ggpubr ggarrange
#' @import ggplot
#'
#' @param dat The input dataset with ranking data.
#' @param target_item A string for the target item's variable name
#' @param other_items A set of strings for variable names corresponding to
#' other items that were available as ranking options
#' @param treat The treatment indicator variable.
#' Defaults to NULL.
#' @param single_plot If TRUE, returns a single plot.
#' If FALSE, returns a list of plots that will compose the combined plot.
#' Defaults to TRUE.
#' @param color_palette The color palette to be used.
#'
#' @return A ggplot that visualizes all treatment effects.
#' If single_plot is TRUE, it will return a single ggplot.
#' If FALSE, it will return four ggplot objects in a list.
#'
#' @examples
#' dat <- read_csv("ex_police.csv")
#' my_target_item <- "victim"
#' my_other_items <-
#'   c("officers", "PDchief", "mayor", "DA", "governor", "senators")
#' vis_ranking(
#'   dat = dat,
#'   treat = my_treat,
#'   target_item = my_target_item,
#'   other_items = my_other_items
#' )
#' @export

vis_ranking <- function(dat,
                        target_item,
                        other_items,
                        treat = NULL,
                        single_plot = TRUE,
                        color_palette = c(
                          "black",
                          "#b0015a",
                          "#128ba0",
                          "gray"
                        )) {
  # Avoiding overwriting utils::data and stats::dt
  # Avoiding reliance on tidyverse as much as possible

  # Check the validity of the input arguments.
  if (!(target_item %in% names(dat))) {
    stop("The target item is not a valid column name in the given datset.")
  }
  if (!all(other_items %in% names(dat))) {
    stop(
      paste0(
        "One or more of other items is not a valid column name",
        " in the given dataset."
      )
    )
  }

  # To-do list: allow target items to be either 1 or 0?
  # Can it be more than 1?

  # Highlight and benchmark color for average rankings and such
  use_col <- c(color_palette[2], color_palette[1])

  # Create a label for items: if there are underbars, turn to spaces
  # Capitalize the first letters
  label <- simple_cap(gsub("_", " ", target_item))

  # Size of the dataset
  N <- nrow(dat)
  J <- length(other_items) + 1

  # Treatment indicator
  if (!is.null(treat)) {
    D <- dat[[treat]]
  }

  # Process the raw ranking data +  store the original data in a separate object
  dat_raw <- dat

  # Prepare a vector and a list to extract quantities of interest
  Y_rank_target <- dat[[target_item]]
  Y_rank_others <- vector("list", length = J - 1)
  for (i in seq(J - 1)) {
    Y_rank_others[[i]] <- dat[[other_items[i]]]
  }

  # Pairwise ranking probability
  Y_pairwise <- vector("list", length = J - 1)
  for (i in seq(J - 1)) {
    compar <- dat[[other_items[i]]] # Comparison item
    Y_pairwise[[i]] <- ifelse(Y_rank_target < compar, 1, 0)
  }

  Y_topk <- seq(J) %>%
    map(~ ifelse(Y_rank_target <= .x, 1, 0))

  Y_marginal <- vector("list", length = J)
  tgt <- dat[[target_item]]
  for (i in seq(J)) {
    Y_marginal[[i]] <- ifelse(tgt == i, 1, 0)
  }

  # Collect estimated means: without treatment
  if (is.null(treat)) {
    # Estimate baseline outcome values via OLS
    m_rank_target <- lm_robust(Y_rank_target ~ 1) %>% tidy()
    m_rank_others <- vector("list", length = J - 1)
    for (i in seq(J - 1)) {
      m_rank_others[[i]] <- lm_robust(Y_rank_others[[i]] ~ 1) %>% tidy()
    }

    m_topk <- seq(J) %>%
      map(~ lm_robust(Y_topk[[.x]] ~ 1) %>% tidy())

    m_pairwise <- vector("list", length = J - 1)
    for (i in seq(J - 1)) {
      m_pairwise[[i]] <- lm_robust(Y_pairwise[[i]] ~ 1) %>% tidy()
    }

    m_marginal <- vector("list", length = J)
    for (i in seq(J)) {
      m_marginal[[i]] <- lm_robust(Y_marginal[[i]] ~ 1) %>% tidy()
    }
    m_rank_catch <- do.call(rbind.data.frame, m_rank_others) %>%
      mutate(
        outcome = paste0(other_items),
        target = "B"
      )
    m_rank <- m_rank_target %>%
      mutate(
        outcome = paste0(target_item),
        target = "A"
      )

    m_rank_catch <- do.call(rbind.data.frame, m_rank_others) %>%
      mutate(
        outcome = paste0(other_items),
        target = "B"
      )
    m_rank <- m_rank_target %>%
      mutate(
        outcome = paste0(target_item),
        target = "A"
      )
    gg_averagerank <- rbind(m_rank, m_rank_catch)

    gg_pairwise <- do.call(rbind.data.frame, m_pairwise) %>%
      mutate(outcome = paste("vs.", other_items))
      ## mutate(outcome = paste("vs.", simple_cap(gsub("_", " ", other_items))))

    gg_marginal <- do.call(rbind.data.frame, m_marginal) %>%
      mutate(outcome = paste("Ranked", seq(J)))

    gg_topk <- do.call(rbind.data.frame, m_topk) %>%
      mutate(outcome = paste0("Top-", seq(J))) %>%
      select(outcome, everything()) %>%
      .[seq(J - 1), ]

    # Visualize all effects
    p_avg <- ggplot(
      gg_averagerank,
      aes(fct_reorder(outcome, desc(estimate)), y = estimate)
    ) +
      geom_point(aes(color = target), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = target),
        linewidth = 1
      )
    p_avg <- vis_helper(p_avg, "avg", J, use_col, label)
    
    p_pair <- ggplot(
      gg_pairwise,
      aes(fct_reorder(outcome, desc(estimate)), y = estimate)
    ) +
      geom_point(size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high),
        linewidth = 1
      ) +
      ylim(0, 1)
    p_pair <- vis_helper(p_pair, "pair", J, use_col, label)

    p_topk <- ggplot(
      gg_topk,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high),
        linewidth = 1
      ) +
      ylim(0, 1)
    p_topk <- vis_helper(p_topk, "topk", J, use_col, label)

    p_marginal <- ggplot(
      gg_marginal,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high),
        linewidth = 1
      ) +
      ylim(0, 1)
    p_marginal <- vis_helper(p_marginal, "marginal", J, use_col, label)

    if (single_plot == TRUE) {
      ggarrange(p_avg, p_pair, p_topk, p_marginal)
    } else {
      return(
        list(
          p_avg = p_avg,
          p_pair = p_pair,
          p_topk = p_topk,
          p_marginal = p_marginal
        )
      )
    }
  } else {
    # Visualization when there is a treatment
    # Prep for visualization
    av_scenario <- scenario <- list(
      c("Not_significant", "Negative", "Positive"),
      c("Not_significant", "Negative"),
      c("Not_significant", "Positive"),
      c("Negative", "Positive"),
      c("Not_significant"),
      c("Negative"),
      c("Positive")
    )
    scena_col <- list(
      c(color_palette[4], color_palette[2], color_palette[3]),
      c(color_palette[4], color_palette[2]),
      c(color_palette[4], color_palette[3]),
      c(color_palette[2], color_palette[3]),
      c(color_palette[4]),
      c(color_palette[2]),
      c(color_palette[3])
    )

    av_scena_col <- list(
      c(color_palette[4], color_palette[3], color_palette[2]),
      c(color_palette[4], color_palette[3]),
      c(color_palette[4], color_palette[2]),
      c(color_palette[3], color_palette[2]),
      c(color_palette[4]),
      c(color_palette[3]),
      c(color_palette[2])
    )

    # Estimate ATEs with Difference-in-means via OLS
    m_rank_target <- lm_robust(Y_rank_target ~ D) %>% tidy()

    m_topk <- seq(J) %>%
      map(~ lm_robust(Y_topk[[.x]] ~ D) %>% tidy())
    
    m_pairwise <- vector("list", length = J - 1)
    for (i in seq(J - 1)) {
      m_pairwise[[i]] <- lm_robust(Y_pairwise[[i]] ~ D) %>% tidy()
    }

    m_marginal <- vector("list", length = J)
    for (i in seq(J)) {
      m_marginal[[i]] <- lm_robust(Y_marginal[[i]] ~ D) %>% tidy()
    }

    m_rank <- m_rank_target %>%
      filter(term == "D") %>%
      mutate(
        outcome = paste0(target_item),
        target = "A"
      )
    gg_averagerank <- rbind(m_rank)

    gg_pairwise <- do.call(rbind.data.frame, m_pairwise) %>%
      filter(term == "D") %>%
      mutate(outcome = paste("vs.", other_items))
      ## mutate(outcome = paste("vs.", simple_cap(gsub("_", " ", other_items))))
    
    gg_marginal <- do.call(rbind.data.frame, m_marginal) %>%
      filter(term == "D") %>%
      mutate(outcome = paste("Ranked", seq(J)))

    gg_topk <- do.call(rbind.data.frame, m_topk) %>%
      filter(term == "D") %>%
      mutate(outcome = paste0("Top-", seq(J)))

    # Visualize all effects
    gg_averagerank$col <- ifelse(
      gg_averagerank$conf.low > 0, "Positive", "Not_significant"
    )
    gg_averagerank$col <- ifelse(
      gg_averagerank$conf.high < 0, "Negative", gg_averagerank$col
    )
    names(av_scena_col) <- av_scenario
    pattern <- unique(gg_averagerank$col) # Observed pattern
    use_col <- av_scena_col[pattern] # Use this color palette

    p_avg <- ggplot(
      gg_averagerank,
      aes(fct_reorder(outcome, desc(estimate)),
        y = estimate
      )
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        linewidth = 1
      )
    p_avg <- vis_helper(p_avg, "avg", J, use_col, label)
    
    gg_pairwise$col <- ifelse(
      gg_pairwise$conf.low > 0, "Positive", "Not_significant"
    )
    gg_pairwise$col <- ifelse(
      gg_pairwise$conf.high < 0, "Negative", gg_pairwise$col
    )
    names(scena_col) <- scenario
    pattern <- unique(gg_pairwise$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color palette

    p_pair <- ggplot(
      gg_pairwise,
      aes(fct_reorder(outcome, desc(estimate)), y = estimate)
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        linewidth = 1
      )
    p_pair <- vis_helper(p_pair, "pair", J, use_col, label)

    gg_topk$col <- ifelse(gg_topk$conf.low > 0, "Positive", "Not_significant")
    gg_topk$col <- ifelse(gg_topk$conf.high < 0, "Negative", gg_topk$col)
    names(scena_col) <- scenario
    pattern <- unique(gg_topk$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color pallet

    p_topk <- ggplot(gg_topk, aes(x = outcome, y = estimate)) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        linewidth = 1
      )
    p_topk <- vis_helper(p_topk, "topk", J, use_col, label)

    gg_marginal$col <- ifelse(
      gg_marginal$conf.low > 0, "Positive", "Not_significant"
    )
    gg_marginal$col <- ifelse(
      gg_marginal$conf.high < 0, "Negative", gg_marginal$col
    )
    names(scena_col) <- scenario
    pattern <- unique(gg_marginal$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color pallet

    p_marginal <- ggplot(
      gg_marginal,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        linewidth = 1
      )
    p_marginal <- vis_helper(p_marginal, "marginal", J, use_col, label)

    if (single_plot == TRUE) {
      ggarrange(p_avg, p_pair, p_topk, p_marginal)
    } else {
      return(
        list(
          p_avg = p_avg,
          p_pair = p_pair,
          p_topk = p_topk,
          p_marginal = p_marginal
        )
      )
    }
  }
}

simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  output <- paste(
    toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " "
  )
  return(output)
}

vis_helper <- function(p, type, J, use_col, label) {
  p <- p +
    ylab("") +
    xlab("") +
    coord_flip() +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
      text = element_text(size = 10),
      plot.title = element_text(size = 10)
    )
  if (tolower(type) == "avg") {
    p <- p +
      scale_colour_manual(values = use_col) +
      ylim(1, J) +
      geom_hline(yintercept = (J + 1) / 2, linetype = "dashed") +
      ggtitle(paste0("A. Average Ranks"))
  } else if (tolower(type) == "pair") {
    p <- p +
      scale_colour_manual(values = use_col) +
      ## geom_hline(yintercept = 0, linetype = "dashed") +
      ggtitle(
        paste0("B. Pairwise Ranking of", " ", label, " ", "Over Other Items")
      )
  } else if (tolower(type) == "topk") {
    p <- p +
      scale_colour_manual(values = use_col) +
      ## geom_hline(yintercept = 0, linetype = "dashed") +
      ggtitle(paste0("C. Top-k Ranking of", " ", label))
  } else if (tolower(type) == "marginal") {
    p <- p +
      scale_colour_manual(values = use_col) +
      ## geom_hline(yintercept = 0, linetype = "dashed") +
      ggtitle(paste0("D. Marginal Ranking of", " ", label))
  }
  
  return(p)
}

