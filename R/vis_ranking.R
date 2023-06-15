#' @description  \code{est_r} visualizes all ATEs in a typical effect set
#'
#' @param data A dataset
#' @param target_item A string for the target item
#' @param other_items A set of strings for other items of interest
#'
#' @return A ggplot that visualizes all treatment effects
#' @examples
#' dt <- read_csv("ex_police.csv")
#' my_target_item <- "victim"
#' my_other_items <- c("officers", "PDchief", "mayor", "DA", "governor", "senators")
#' vis_ranking(
#'   data = dt,
#'   treat = my_treat,
#'   target_item = my_target_item,
#'   other_items = my_other_items
#' )
#' @export

vis_ranking <- function(data,
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
  use_col <- c(color_palette[2], color_palette[1])
  label <- simple_cap(gsub("_", " ", simple_cap(target_item)))

  # Size
  N <- dim(data)[1]

  # Treatment indicator
  if (!is.null(treat)) {
    D <- data[treat] %>% pull()
  }

  # Process raw ranking data
  J_1 <- length(other_items) # J - 1
  J <- J_1 + 1

  dt <- data

  Y_rank_target <- dt[target_item] %>% pull() # Average ranks
  Y_rank_others <- list()
  for (i in 1:J_1) {
    Y_rank_others[[i]] <- dt[other_items[i]] %>% pull()
  }

  Y_pairwise <- list() # Pairwise ranking P
  for (i in 1:J_1) {
    compar <- dt[other_items[i]] %>% pull() # Comparison item
    Y_pairwise[[i]] <- ifelse(Y_rank_target < compar, 1, 0)
  }

  Y_top1 <- ifelse(Y_rank_target <= 1, 1, 0) # Top-1 ranking Probability (P)
  Y_top2 <- ifelse(Y_rank_target <= 2, 1, 0) # Top-2 ranking P
  Y_top3 <- ifelse(Y_rank_target <= 3, 1, 0) # Top-3 ranking P
  Y_top4 <- ifelse(Y_rank_target <= 4, 1, 0) # Top-4 ranking P
  Y_top5 <- ifelse(Y_rank_target <= 5, 1, 0) # Top-5 ranking P
  Y_top6 <- ifelse(Y_rank_target <= 6, 1, 0) # Top-6 ranking P
  Y_top7 <- ifelse(Y_rank_target <= 7, 1, 0) # Top-7 ranking P

  Y_marginal <- list()
  tgt <- dt[target_item] %>% pull()
  for (i in 1:J) {
    Y_marginal[[i]] <- ifelse(tgt == i, 1, 0)
  }

  # Collect estimated means: without treatment
  if (is.null(treat)) {
    # Estimate baseline outcome values via OLS
    m_rank_target <- lm_robust(Y_rank_target ~ 1) %>% tidy()
    m_rank_others <- list()
    for (i in 1:J_1) {
      m_rank_others[[i]] <- lm_robust(Y_rank_others[[i]] ~ 1) %>% tidy()
    }

    m_top1 <- lm_robust(Y_top1 ~ 1) %>% tidy()
    m_top2 <- lm_robust(Y_top2 ~ 1) %>% tidy()
    m_top3 <- lm_robust(Y_top3 ~ 1) %>% tidy()
    m_top4 <- lm_robust(Y_top4 ~ 1) %>% tidy()
    m_top5 <- lm_robust(Y_top5 ~ 1) %>% tidy()
    m_top6 <- lm_robust(Y_top6 ~ 1) %>% tidy()
    m_top7 <- lm_robust(Y_top7 ~ 1) %>% tidy()

    m_pairwise <- list()
    for (i in 1:J_1) {
      m_pairwise[[i]] <- lm_robust(Y_pairwise[[i]] ~ 1) %>% tidy()
    }

    m_marginal <- list()
    for (i in 1:J) {
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
      mutate(outcome = paste0("v.", " ", other_items))

    gg_marginal <- do.call(rbind.data.frame, m_marginal) %>%
      mutate(outcome = paste0("Ranked", " ", 1:J))

    gg_topk <- rbind(
      m_top1, m_top2, m_top3, m_top4,
      m_top5, m_top6, m_top7
    ) %>%
      mutate(
        outcome = c(
          "Top-1", "Top-2", "Top-3", "Top-4",
          "Top-5", "Top-6", "Top-7"
        )
      ) %>%
      select(outcome, everything()) %>%
      ## Limit to less than 7 if J<7
      .[1:(J - 1), ]

    # Visualize all effects
    p_avg <- ggplot(
      gg_averagerank,
      aes(fct_reorder(outcome, desc(estimate)),
        y = estimate
      )
    ) +
      geom_point(aes(color = target), size = 2) +
      geom_linerange(
        aes(
          x = outcome, ymin = conf.low, ymax = conf.high,
          color = target
        ),
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      ylab("") +
      xlab("") +
      ylim(1, (1 + J_1)) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("A. Average Ranks")) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    p_pair <- ggplot(
      gg_pairwise,
      aes(fct_reorder(outcome, desc(estimate)),
        y = estimate
      )
    ) +
      geom_point(aes(), size = 2) +
      geom_linerange(aes(x = outcome, ymin = conf.low, ymax = conf.high),
        lwd = 1
      ) +
      ylab("") +
      xlab("") +
      ylim(0, 1) +
      coord_flip() +
      theme_bw() +
      ggtitle(
        paste0("B. Pairwise Ranking of", " ", label, " ", "Over Other Options")
      ) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    p_topk <- ggplot(
      gg_topk,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(), size = 2) +
      geom_linerange(aes(x = outcome, ymin = conf.low, ymax = conf.high),
        lwd = 1
      ) +
      ylab("") +
      xlab("") +
      ylim(0, 1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("C. Top-k Ranking of", " ", label)) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    p_marginal <- ggplot(
      gg_marginal,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(), size = 2) +
      geom_linerange(aes(x = outcome, ymin = conf.low, ymax = conf.high),
        lwd = 1
      ) +
      ylab("") +
      xlab("") +
      ylim(0, 1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("D. Marginal Ranking of", " ", label)) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    if (single_plot == TRUE) {
      ggpubr::ggarrange(p_avg, p_pair, p_topk, p_marginal)
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
    # Prep for visualization
    scenario <- list(
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

    av_scenario <- list(
      c("Not_significant", "Negative", "Positive"),
      c("Not_significant", "Negative"),
      c("Not_significant", "Positive"),
      c("Negative", "Positive"),
      c("Not_significant"),
      c("Negative"),
      c("Positive")
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

    m_top1 <- lm_robust(Y_top1 ~ D) %>% tidy()
    m_top2 <- lm_robust(Y_top2 ~ D) %>% tidy()
    m_top3 <- lm_robust(Y_top3 ~ D) %>% tidy()
    m_top4 <- lm_robust(Y_top4 ~ D) %>% tidy()
    m_top5 <- lm_robust(Y_top5 ~ D) %>% tidy()
    m_top6 <- lm_robust(Y_top6 ~ D) %>% tidy()
    m_top7 <- lm_robust(Y_top7 ~ D) %>% tidy()

    m_pairwise <- list()
    for (i in 1:J_1) {
      m_pairwise[[i]] <- lm_robust(Y_pairwise[[i]] ~ D) %>% tidy()
    }

    m_marginal <- list()
    for (i in 1:J) {
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
      mutate(outcome = paste0("v.", " ", other_items))

    gg_marginal <- do.call(rbind.data.frame, m_marginal) %>%
      filter(term == "D") %>%
      mutate(outcome = paste0("Ranked", " ", 1:J))

    gg_topk <- rbind(
      m_top1, m_top2, m_top3, m_top4,
      m_top5, m_top6, m_top7
    ) %>%
      filter(term == "D") %>%
      mutate(outcome = c(
        "Top-1", "Top-2", "Top-3", "Top-4",
        "Top-5", "Top-6", "Top-7"
      ))

    # Visualize all effects
    gg_averagerank$col <- ifelse(
      gg_averagerank$conf.low > 0, "Positive", "Not_significant"
    )
    gg_averagerank$col <- ifelse(
      gg_averagerank$conf.high < 0, "Negative", gg_averagerank$col
    )
    names(av_scena_col) <- av_scenario
    pattern <- unique(gg_averagerank$col) # Observed pattern
    use_col <- av_scena_col[pattern] # Use this color pallet

    p_avg <- ggplot(
      gg_averagerank,
      aes(fct_reorder(outcome, desc(estimate)),
        y = estimate
      )
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_colour_manual(values = use_col) +
      ylab("") +
      xlab("") +
      #  ylim(1,(1+J_1)) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("A. Average Ranks")) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    gg_pairwise$col <- ifelse(
      gg_pairwise$conf.low > 0, "Positive", "Not_significant"
    )
    gg_pairwise$col <- ifelse(
      gg_pairwise$conf.high < 0, "Negative", gg_pairwise$col
    )
    names(scena_col) <- scenario
    pattern <- unique(gg_pairwise$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color pallet

    p_pair <- ggplot(
      gg_pairwise,
      aes(fct_reorder(outcome, desc(estimate)),
        y = estimate
      )
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      ylab("") +
      xlab("") +
      #  ylim(0,1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("B. Pairwise Ranking of", " ", target_item, " ", "Over")) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    gg_topk$col <- ifelse(gg_topk$conf.low > 0, "Positive", "Not_significant")
    gg_topk$col <- ifelse(gg_topk$conf.high < 0, "Negative", gg_topk$col)
    names(scena_col) <- scenario
    pattern <- unique(gg_topk$col) # Observed pattern
    use_col <- scena_col[pattern] # Use this color pallet

    p_topk <- ggplot(
      gg_topk,
      aes(x = outcome, y = estimate)
    ) +
      geom_point(aes(color = col), size = 2) +
      geom_linerange(
        aes(x = outcome, ymin = conf.low, ymax = conf.high, color = col),
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      ylab("") +
      xlab("") +
      #  ylim(0,1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("C. Top-k Ranking of", " ", target_item)) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

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
        lwd = 1
      ) +
      scale_colour_manual(values = use_col) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      ylab("") +
      xlab("") +
      #  ylim(0,1) +
      coord_flip() +
      theme_bw() +
      ggtitle(paste0("D. Marginal Ranking of", " ", target_item)) +
      theme(
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0.2, -0.2, "cm"),
        text = element_text(size = 10),
        plot.title = element_text(size = 10)
      )

    if (single_plot == TRUE) {
      ggpubr::ggarrange(p_avg, p_pair, p_topk, p_marginal)
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
