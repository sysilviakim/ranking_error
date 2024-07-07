# Library and function calls ===================================================
library(plyr)
library(MASS)
library(here)
library(combinat) # For permn
library(gtools) # For permute
library(tidyselect)
library(tidyverse)
library(styler)
library(PLMIX) # For switch_ord_rank()
library(patchwork)
library(janitor) # For clean_names
library(scales) # For percent
library(assertthat)
library(pwr)
library(xtable)
library(estimatr)
library(ggpubr)
# remotes::install_github("sysilviakim/Kmisc")
library(Kmisc) ## None-CRAN package. Will wean eventually
library(lubridate)
library(haven)
## remotes::install_github("sysilviakim/ranking")
library(ranking)

# Read all functions
source(here("R", "yougov_import.R"))

# Parameters/stored vectors ====================================================
bootstrap_n <- 1000
root_var <- c(
  tate = "123",
  identity = "1234",
  polar = "12345",
  esystems = "123456"
)
## Previously...
# x <- c(
#   `1` = "Gender", `2` = "City", `3` = "Country", `4` = "Socioeconomic Status",
#   `5` = "Racial or Ethnic Group", `6` = "Political Party", `7` = "Religion"
# )
# match(x, sort(x)) %>% paste(collapse = "") ## 3127546

option_crosswalk <- c(
  ## Tate
  policy = "app_tate_1",
  pork = "app_tate_2",
  service = "app_tate_3",
  ## Electoral systems
  account_pol = "app_esystems_1",
  account_gov = "app_esystems_2",
  vote_seat = "app_esystems_3",
  women = "app_esystems_4",
  minority = "app_esystems_5",
  policy_match = "app_esystems_6",
  ## Identity
  party = "app_identity_1",
  religion = "app_identity_2",
  gender = "app_identity_3",
  race = "app_identity_4",
  ## Polarization
  politicians = "app_polar_1",
  print_media = "app_polar_2",
  social_media = "app_polar_3",
  interest_groups = "app_polar_4",
  citizens = "app_polar_5"
)

# Functions ====================================================================
## Turn 1st, 2nd, and 3rd ranking columns into long data format
pivot_sim <- function(x) {
  out <- x %>%
    pivot_longer(
      cols = c(V1, V2, V3), names_to = "position", values_to = "item"
    ) %>%
    mutate(
      rank = case_when(
        position == "V1" ~ str_sub(obs_rank, 1, 1), # First value
        position == "V2" ~ str_sub(obs_rank, 2, 2), # Second value
        position == "V3" ~ str_sub(obs_rank, 3, 3) # Third value
      )
    ) %>%
    arrange(id, rank)
  return(out)
}

loop_stated_rank_preference <- function(true_rank, choice) {
  # Storage for observed patterns
  obs_pattern <- data.frame(
    pattern1 = integer(),
    pattern2 = integer(),
    pattern3 = integer()
  )

  # For each i-th unit in true_rank
  for (i in 1:dim(true_rank)[1]) {
    # Cast numbers to alphabets
    vec_pref <- true_rank[i, ] %>%
      pivot_longer(cols = c(a, b, c), names_to = "variable")
    vec_pref # Check

    # Alphabet unit i sees in each position
    position1 <- choice[i, 1] ## %>% pull()
    position2 <- choice[i, 2] ## %>% pull()
    position3 <- choice[i, 3] ## %>% pull()

    # Assign a value (rank) for each position
    # given the alphabet unit i sees there
    pattern1 <- vec_pref$value[vec_pref$variable == position1]
    pattern2 <- vec_pref$value[vec_pref$variable == position2]
    pattern3 <- vec_pref$value[vec_pref$variable == position3]

    # Combine the result
    comb <- data.frame(
      pattern1 = pattern1,
      pattern2 = pattern2,
      pattern3 = pattern3
    )

    # Stack in the storage
    obs_pattern <- rbind(obs_pattern, comb)

    # End of i loop
  }

  obs_pattern <- obs_pattern %>% unite(obs_rank, sep = "")
  return(as_tibble(obs_pattern))
}

prop_vector <- function(x, digits = 1) {
  return(round(prop.table(table(x, useNA = "ifany")), digits = digits))
}

rowid <- function(x) {
  out <- x %>%
    mutate(id = row_number()) %>%
    select(id, everything())
  return(out)
}

pivot_join <- function(x, y) {
  return(pivot_sim(left_join(y, x)) %>% select(id, obs_rank, item, rank))
}

## Print effect size and power
chisq_power <- function(tab, power = 0.95) {
  ## Chi-square test
  message("The chi-square test result is:")
  print(chisq.test(tab, p = rep(1 / length(tab), length(tab))))

  ## Power test. We could do the power = 0.8, sure
  P0 <- rep(1 / length(tab), length(tab))
  P1 <- as.numeric(prop.table(tab))
  message(paste0("Effect size is ", ES.w1(P0, P1)))
  if (all(P0 == P1)) {
    message("Everything is equally distributed.")
  } else {
    message("The chi-square power test result is:")
    print(
      pwr.chisq.test(w = ES.w1(P0, P1), df = (length(tab) - 1), power = power)
    )
  }
}

## Collapse into resulting ranking pattern in the permutation space
unite_ranking <- function(x, remove = FALSE) {
  x_raw <- x

  ## Find all patterns and bring it to its "root," i.e., without the _1
  var_list <- x %>%
    select(ends_with("_1")) %>%
    names() %>%
    str_sub(end = -3)

  ## Perform for each pattern
  for (v in var_list) {
    ## Is it 3-option, 4-option, 7, 8, ...?
    l <- x %>%
      select(contains(v)) %>%
      select(
        -ends_with("timing"), 
        -ends_with("_rnd")
      )

    if (!grepl("repeat", v)) {
      l <- l %>%
        select(-contains("repeat")) %>%
        ncol()
    } else {
      l <- l %>%
        select(contains("repeat")) %>%
        ncol()
    }

    x <- x %>%
      unite(
        !!as.name(v),
        sep = "", !!as.name(paste0(v, "_1")):!!as.name(paste0(v, "_", l)),
        remove = remove
      ) %>%
      mutate(
        !!as.name(v) := case_when(
          grepl("NANA", !!as.name(v)) ~ NA_character_,
          TRUE ~ !!as.name(v)
        )
      )
  }

  return(x)
}

avg_rank_bootstrap_quantile <- function(x) {
  if (class(x) != "list") {
    stop("Input is not a list.")
  }
  x %>%
    imap(
      ~ .x %>%
        pivot_longer(everything(), names_to = "variable") %>%
        group_by(variable) %>%
        summarize(
          mean_val = mean(value),
          low = quantile(value, probs = 0.025),
          up = quantile(value, probs = 0.975)
        ) %>%
        mutate(Estimator = .y)
    ) %>%
    bind_rows() %>%
    mutate(
      Estimator = case_when(
        Estimator == "debiased" ~ "De-biased",
        TRUE ~ "Naive"
      )
    )
}

cor_and_condprob <- function(df, v1, v2) {
  if (!(v1 %in% names(df)) | !(v2 %in% names(df))) {
    stop("One or more of the variables does not exist in the dataframe.")
  }
  ## Something like with(df, table(tate = v1, identity = v2)) fails
  ## Not much point in printing table, I guess...
  print(cor(df[[v1]], df[[v2]], use = "pairwise"))
  pretty_condprob(df, v1, 1, v2, 1)
  pretty_condprob(df, v2, 1, v1, 1)
}

transitivity_pattern <- function(x) {
  if (is.na(x)) {
    out <- NA_character_
  } else {
    split_x <- substring(x, seq(nchar(x)), seq(nchar(x)))
    out <- seq(0, length(split_x)) %>%
      map(~ append(split_x, as.character(length(split_x) + 1), .x)) %>%
      map(~ paste(.x, collapse = "")) %>%
      unlist()
  }
  return(out)
}

repeat_coherent <- function(x, v1, v2) {
  y1 <- sum(x[[v1]] == x[[v2]], na.rm = TRUE)
  y2 <- sum(x[[v1]] != x[[v2]], na.rm = TRUE)
  return(
    paste0(
      "Repeated task for ", v1, " yields a ",
      round(y1 / (y1 + y2) * 100, digits = 1), "% rate of same answers."
    )
  )
}

venn_diagram_fill <- function(x, v1, v2, v3) {
  ## This is a slapdash function to fill in the Lucidchart Venn diagram
  ## manually; use at discretion
  list(
    x1 = c(1, 0, 0),
    x2 = c(1, 1, 0),
    x3 = c(1, 1, 1),
    x4 = c(1, 0, 1),
    x5 = c(0, 1, 0),
    x6 = c(0, 1, 1),
    x7 = c(0, 0, 1),
    x8 = c(0, 0, 0)
  ) %>%
    map_chr(
      ~ main %>%
        filter(
          !!as.name(v1) == .x[1] &
            !!as.name(v2) == .x[2] &
            !!as.name(v3) == .x[3]
        ) %>%
        nrow() %>%
        {
          formatC(
            round(
              . / nrow(
                main %>%
                  filter(
                    !is.na(!!as.name(v1)) &
                      !is.na(!!as.name(v2)) &
                      !is.na(!!as.name(v3))
                  )
              ) * 100,
              digits = 1
            ),
            digits = 1,
            format = "f"
          )
        }
    )
}

## Not perfectly do-not-repeat-yourself but will return later
pattern_compare_pass_fail <- function(main, v, y_upper = .75, label = NULL,
                                      recorded = TRUE) {
  if (recorded) {
    sfx <- "_recorded"
  } else {
    sfx <- ""
  }
  if (is.null(label)) {
    label <- c("Passed", "Failed")
  }
  out <- root_var %>%
    imap(
      function(x, y) {
        list(
          pass = list(v = 0, lab = label[1]),
          fail = list(v = 1, lab = label[2])
        ) %>%
          map(
            ~ main %>%
              filter(!!as.name(v) == .x$v) %>%
              .[[paste0("app_", y, sfx)]] %>%
              table() %>%
              table_to_tibble() %>%
              plot_dist_ranking(., ylim = y_upper) +
              ggtitle(.x$lab)
          ) %>%
          map(~ plot_nolegend(pdf_default(.x)))
      }
    )

  ## Supplement by no-context questions
  p3 <- list(
    pass = list(v = 0, lab = "Passed"),
    fail = list(v = 1, lab = "Failed")
  ) %>%
    map(
      ~ main %>%
        filter(!is.na(no_context_3_recorded)) %>%
        filter(!!as.name(v) == .x$v) %>%
        .$no_context_3_recorded %>%
        table() %>%
        table_to_tibble() %>%
        plot_dist_ranking(., ylim = y_upper) +
        ggtitle(.x$lab)
    ) %>%
    map(~ plot_nolegend(pdf_default(.x)))

  p4 <- list(
    pass = list(v = 0, lab = "Passed"),
    fail = list(v = 1, lab = "Failed")
  ) %>%
    map(
      ~ main %>%
        filter(!is.na(no_context_4_recorded)) %>%
        filter(!!as.name(v) == .x$v) %>%
        .$no_context_4_recorded %>%
        table() %>%
        table_to_tibble() %>%
        plot_dist_ranking(., ylim = y_upper) +
        ggtitle(.x$lab)
    ) %>%
    map(~ plot_nolegend(pdf_default(.x)))

  out$no_context_3 <- p3
  out$no_context_4 <- p4

  return(out)
}

## One-time functions ----------------------------------------------------------
ggsave_temp <- function(x, width = 5.5, height = 3) {
  ggsave(here("fig", x), width = width, height = height)
}

pdf_short <- function(p) {
  pdf_default(p) +
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()
    )
}

crosswalk_short <- function(x, label) {
  x %>%
    bind_rows(.id = "topic") %>%
    rowwise() %>%
    mutate(varname = paste0("app_", label, "_", substr(name, 1, 1))) %>%
    left_join(
      ., tibble(item = names(option_crosswalk), varname = option_crosswalk)
    ) %>%
    filter(grepl(topic, varname)) %>%
    ungroup() %>%
    select(topic, varname, item, everything()) %>%
    select(-name) %>%
    ## Must refactor code
    mutate(
      name = item, 
      est = mean,
      low = mean - 1.96 * se,
      up = mean + 1.96 * se,
      imp = ""
    )
}

viz_avg_wrapper <- function(data, J = NULL) {
  if (nrow(data) / 2 == 3) {
    order <- c("Policy", "Pork", "Service")
  } else if (nrow(data) / 2 == 4) {
    order <- c("Gender", "Religion", "Race", "Party")
  } else if (nrow(data) / 2 == 5) {
    order <- c(
      "Politicians", "Print Media and TV",
      "Social Media", "Interest Groups", "Citizens"
    )
  } else if (nrow(data) / 2 == 6) {
    order <- c(
      "Accountability (Politicians)", "Accountability (Government)",
      "Median Voter Policy", "Seat-Vote Unbiased", 
      "Minority Representation", "Women's Representation"
    )
  } else {
    stop("Invalid number of rows.")
  }
  data <- data %>%
    rowwise() %>%
    mutate(
      name = simple_cap(name),
      name = case_when(
        name == "Print_media" ~ "Print Media and TV",
        name == "Social_media" ~ "Social Media",
        name == "Interest_groups" ~ "Interest Groups",
        name == "Account_pol" ~ "Accountability (Politicians)",
        name == "Account_gov" ~ "Accountability (Government)",
        name == "Policy_match" ~ "Median Voter Policy",
        name == "Vote_seat" ~ "Seat-Vote Unbiased",
        name == "Women" ~ "Women's Representation",
        name == "Minority" ~ "Minority Representation",
        TRUE ~ name
      )
    ) %>%
    ungroup()
  p <- viz_avg_rank(data, order, J = J, label_pad_width = 28)
  return(
    pdf_default(p) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(-0.5, 0, 0, 0, unit = "cm"),
        legend.spacing.x = unit(0, "cm"),
        legend.spacing.y = unit(0, "cm"),
        plot.title = element_text(face = "bold")
      )
  )
}

## Must make it more general
summ_avg_rank_by_group <- function(data, items = NULL, 
                                   v = NULL, label = NULL, raw = TRUE,
                                   weight = NULL, round = 2,
                                   mean_only = TRUE) {
  if (is.null(v) | is.null(label)) {
    out <- avg_rank(
      data, "app_polar", items = items,
      raw = raw, weight = weight, round = round
    )
    if (mean_only) {
      return(
        out %>%
          select(-se, -qoi, -lower, -upper, -method) %>%
          pivot_wider(names_from = "name", values_from = "mean") %>%
          select(
            Politicians, `Print Media and TV`,
            `Social Media`, `Interest Groups`, Citizens
          )
      )
    } else {
      return(out)
    }
    
  } else {
    out <- unique(data[[v]]) %>%
      set_names(., .) %>%
      map(
        ~ avg_rank(
          data %>% filter(!!as.name(v) == .x), "app_polar",
          items = items, raw = raw, weight = weight, round = round
        )
      ) %>%
      bind_rows(.id = label)
    if (mean_only) {
      return(
        out %>%
          select(-se, -qoi, -lower, -upper, -method) %>%
          pivot_wider(names_from = "item", values_from = "mean") %>%
          select(
            !!as.name(label), Politicians, `Print Media and TV`,
            `Social Media`, `Interest Groups`, Citizens
          )
      )
    } else {
      return(out)
    }
  }
}

viz_avg_rank_temp <- function(data, wrap = NULL, ipw_only = FALSE) {
  p <- ggplot(data %>% rename(Method = method), aes(x = mean, y = item)) +
    geom_vline(xintercept = 3, linetype = "dashed", color = "gray50") +
    geom_point(aes(color = Method, shape = Method),
      position = position_dodge(width = 0.5)
    ) +
    geom_errorbar(
      aes(
        xmin = lower, xmax = upper,
        color = Method, linetype = Method
      ),
      width = 0,
      position = position_dodge(width = 0.5)
    ) +
    scale_color_manual(
      values = c("#b0015a", "#999999"),
      name = "Method", labels = c("IPW", "Raw Data")
    ) +
    scale_shape_manual(
      name = "Method", labels = c("IPW", "Raw Data"),
      values = c(17, 15)
    ) +
    scale_linetype_manual(
      name = "Method", labels = c("IPW", "Raw Data"),
      values = c(2, 3)
    ) +
    xlim(1.8, 5) +
    ylab("") + 
    xlab("Average Rank") +
    theme_bw() + 
    scale_x_continuous(limits = c(0.75, 5.25))
  if (ipw_only) {
    p <- p +
      scale_shape_manual(
        name = "Method", labels = c("IPW", "Raw Data"),
        values = rev(c(16, 16))
      ) +
      scale_linetype_manual(
        name = "Method", labels = c("IPW", "Raw Data"),
        values = rev(c("solid", "solid"))
      )
  }
  
  if (!is.null(wrap)) {
    p <- p + facet_wrap(as.formula(paste("~", wrap)))
  }
  return(p)
}

coef_rename <- function(x) {
  ## List input
  x %>%
    ## Rename variables
    map(
      ~ {
        names(.x$coefficients) <- names(.x$coefficients) %>%
          str_replace_all("age", "Age") %>%
          str_replace_all("Age_sq", "Age squared") %>%
          str_replace_all("gender3Female", "Female") %>%
          str_replace_all("gender3Other", "Neither male/female") %>%
          str_replace_all("race4labeledBlack", "Black") %>%
          str_replace_all("race4labeledHispanic", "Hispanic/Latino") %>%
          str_replace_all("race4labeledOther race", "Other race") %>%
          str_replace_all("educ4Some college", "Some college") %>%
          str_replace_all("educ4College grad", "College graduate") %>%
          str_replace_all("educ4Postgrad", "Post-graduate") %>%
          str_replace_all("pid3finalIndependent", "Independent") %>%
          str_replace_all("pid3finalRepublican", "Republican") %>%
          str_replace_all("ideo7", "Ideology") %>%
          str_replace_all("newsint", "Political interest") %>%
          str_replace_all("regionSouth", "Region: south") %>%
          str_replace_all("regionWest", "Region: west") %>%
          str_replace_all("regionMidwest", "Region: midwest") %>%
          str_replace_all("ideo7Moderate", "Moderate") %>%
          str_replace_all("faminc50-80k", "Household income: 50-80k") %>%
          str_replace_all("faminc80-150k", "Household income: 80-150k") %>%
          str_replace_all(
            "faminc150k or more",
            "Household income: 150k or more"
          ) %>%
          str_replace_all(
            "famincPrefer not to say",
            "Household income: prefer not to say"
          ) %>%
          str_replace_all("relig5Catholic", "Catholic") %>%
          str_replace_all("relig5Jewish", "Jewish") %>%
          str_replace_all(
            "relig5None/Agnostic/Atheist",
            "None/agnostic/atheist"
          ) %>%
          str_replace_all("relig5Other", "Other religion") %>%
          str_replace_all("party_intense", "Party ID intensity")
        return(.x)
      }
    ) %>%
    map(
      ~ list(
        model = .x,
        se_robust = coeftest(.x, vcov = vcovHC(.x, type = "HC0"))[, "Std. Error"]
      )
    )
}


regress_clarify_temp <- function(v, n, p_qoi, type = 1) {
  for (i in 1:n) {
    e_XB_party <- exp(
      v$`(Intercept):party` + v$`ideo7:party` * i +
        v$`pid7:party` * .fix_pid +
        v$`male:party` * .fix_male +
        v$`age:party` * .fix_age +
        v$`educ:party` * .fix_edu)
    e_XB_race <- exp(
      v$`(Intercept):race` + v$`ideo7:race` * i +
        v$`pid7:race` * .fix_pid +
        v$`male:race` * .fix_male +
        v$`age:race` * .fix_age +
        v$`educ:race` * .fix_edu)
    e_XB_reli <- exp(
      v$`(Intercept):religion` + v$`ideo7:religion` * i +
        v$`pid7:religion` * .fix_pid +
        v$`male:religion` * .fix_male +
        v$`age:religion` * .fix_age +
        v$`educ:religion` * .fix_edu)
    e_XB_gen <- 1
    
    # Here, we want to compute the probability for one unique ranking
    # Prob (party, race, religion, gender)
    # Prob(party) * Prob(race) * Prob(religion) * Prob(gender)
    # This is multiplication of three multinomial choices
    if (type == 1) {
      ## Gender > Race > Party > Religion
      p <- e_XB_gen / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
        e_XB_race / (e_XB_race + e_XB_reli + e_XB_party) *
        e_XB_party / (e_XB_reli + e_XB_party) *
        e_XB_reli / e_XB_reli
    } else if (type == 2) {
      ## Party > Gender > Race > Religion
      p <- e_XB_party / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
        e_XB_gen / (e_XB_race + e_XB_gen + e_XB_reli) *
        e_XB_race / (e_XB_race + e_XB_reli) *
        e_XB_reli / e_XB_reli
    } else if (type == 3) {
      ## Gender > Race > Religion > Party
      p <- e_XB_gen / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
        e_XB_race / (e_XB_race + e_XB_reli + e_XB_party) *
        e_XB_reli / (e_XB_reli + e_XB_party) *
        e_XB_party / e_XB_party
    } else if (type == 4) {
      ## Religion > Gender > Race > Party
      p <- e_XB_reli / (e_XB_party + e_XB_race + e_XB_reli + e_XB_gen) *
        e_XB_gen / (e_XB_race + e_XB_gen + e_XB_party) *
        e_XB_race / (e_XB_race + e_XB_party) *
        e_XB_party / e_XB_party
    }
    
    # we want to generate 24 ps. They should sum up to one.
    p_qoi[i, 2] <- mean(p)
    # if we bootstrap the whole thing, we don't need to save this
    p_qoi[i, 3] <- quantile(p, prob = 0.025)
    p_qoi[i, 4] <- quantile(p, prob = 0.975)
  }
  return(p_qoi)
}

