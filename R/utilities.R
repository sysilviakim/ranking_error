## If these none-CRAN packages have not been installed, try this code
# library(remotes)
# install_github(
#   "sysilviakim/Kmisc",
#   INSTALL_opts = c("--no-multiarch"),
#   dependencies = TRUE
# )
# install_github(
#   "sysilviakim/ranking",
#   INSTALL_opts = c("--no-multiarch"),
#   dependencies = TRUE
# )

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
library(Kmisc) ## None-CRAN package
library(lubridate)
library(haven)
library(ranking) ## None-CRAN package
library(questionr)
library(RColorBrewer)
library(clarify)
library(mlogit)
library(future.apply)
library(lmtest)
library(sandwich)
library(stargazer)
library(fixest)
library(coefplot)

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
      data, "app_polar",
      items = items,
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
        v$`educ:party` * .fix_edu
    )
    e_XB_race <- exp(
      v$`(Intercept):race` + v$`ideo7:race` * i +
        v$`pid7:race` * .fix_pid +
        v$`male:race` * .fix_male +
        v$`age:race` * .fix_age +
        v$`educ:race` * .fix_edu
    )
    e_XB_reli <- exp(
      v$`(Intercept):religion` + v$`ideo7:religion` * i +
        v$`pid7:religion` * .fix_pid +
        v$`male:religion` * .fix_male +
        v$`age:religion` * .fix_age +
        v$`educ:religion` * .fix_edu
    )
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

# Functionalize bootstrapping with Plackett-Luce
boot_luce_temp <- function(data,
                           boot_seed) {
  set.seed(boot_seed)

  ## Otherwise, this part is not reproducible
  index <-
    sample(1:nrow(data), size = nrow(data), replace = TRUE)
  bootstrap_dat <- data[index, ]

  boot_ipw <- imprr_weights(
    data = bootstrap_dat,
    J = 4,
    main_q = "app_identity",
    anc_correct = "anc_correct_identity",
    seed = boot_seed
  )

  # Bootstrap data with estimated weights
  bootstrap_dat <- bootstrap_dat %>%
    left_join(boot_ipw$weights, by = "ranking") %>%
    mutate(w_multiplied = weight * w)

  # 2. Data processing =========================================================
  # Reference set: (party, religion, gender, race)
  # Downsize data
  dt <- bootstrap_dat %>%
    mutate(
      id = 1:nrow(bootstrap_dat),
      race = as.factor(race4),
      region = as.factor(region)
    ) %>%
    rename(
      ch.party = app_identity_1,
      ch.religion = app_identity_2,
      ch.gender = app_identity_3,
      ch.race = app_identity_4
    ) %>%
    ## Use race4 instead of race6 that was previously used
    drop_na()

  # 3. Create indexed data =====================================================
  # Transform into mlogit format
  dt <- as.data.frame(dt)

  ## Takes about 2 seconds
  mdat <- dfidx::dfidx(
    dt,
    shape = "wide",
    choice = "ch",
    varying = 1:4, # 1:J, J = # items
    ranked = TRUE
  )
  # Check
  head(mdat)

  # print(b)
  # mdat$ch # logical: TRUE if id2 was ranked idx1-th, by unit id1
  # mdat$id # respondent id
  # mdat$idx # position id (1st, 2nd, 3rd, 4th = depressed)
  # mdat$ideo7 # explanatory variable

  # 4. Run Rank-order logit model ==============================================
  # Estimating parameters (with weight)
  m2 <- mlogit(
    ch ~ 1 | ideo7 + pid7 + partisan + male + age + race4 + educ + region,
    mdat, # Data
    reflevel = "gender", # Base category
    weight = w_multiplied # Survey weights X bias-correction weights
  )

  # Raw result
  summary(m2)

  # 5. Get predicted probabilities =============================================
  ## 5.1. Gender > Race > Party > Religion =====================================
  # Generate 1000 sets of parameters (parametric bootstrap)
  ## First, reusable template
  p_template <- data.frame(
    ideology = 1:7,
    mean = NA,
    low = NA,
    up = NA
  )

  # Generate 1000 sets of parameters (parametric bootstrap)
  # set.seed(123)
  sim_coefs <- sim(m2)
  v <- sim_coefs$sim.coefs %>% as_tibble()
  ggdt1 <- regress_clarify_temp(v, 7, p_template, type = 1) %>%
    mutate(ranking = "Pr(gender > race > party > religion)")

  ## 5.2. Party > Gender > Race > Religion =====================================
  # Generate 1000 sets of parameters (parametric bootstrap)
  # set.seed(123)
  sim_coefs <- sim(m2)
  v <- sim_coefs$sim.coefs %>% as_tibble()
  ggdt2 <- regress_clarify_temp(v, 7, p_template, type = 2) %>%
    mutate(ranking = "Pr(party > gender > race > religion)")

  ## 5.3. Gender > Race > Party > Religion =====================================
  # Generate 1000 sets of parameters (parametric bootstrap)
  # set.seed(123)
  sim_coefs <- sim(m2)
  v <- sim_coefs$sim.coefs %>% as_tibble()
  ggdt3 <- regress_clarify_temp(v, 7, p_template, type = 3) %>%
    mutate(ranking = "Pr(gender > race > religion > party)")

  ## 5.4. Religion > Gender > Race > Party  ====================================
  # Generate 1000 sets of parameters (parametric bootstrap)
  # set.seed(123)
  sim_coefs <- sim(m2)
  v <- sim_coefs$sim.coefs %>% as_tibble()
  ggdt4 <- regress_clarify_temp(v, 7, p_template, type = 4) %>%
    mutate(ranking = "Pr(religion > gender > race > party)")

  # Tie everything together
  ggdt_ipw <- rbind(ggdt1, ggdt2, ggdt3, ggdt4)
  return(ggdt_ipw)
}

## import and wrangle YouGov data
yougov_import <- function(fname) {
  if (grepl(".csv", fname)) {
    df_raw <- read_csv(here("data", "raw", fname), show_col_types = FALSE)
  } else {
    df_raw <- read_sav(here("data", "raw", fname))
  }
  ## Exported using not the choice text but numeric values
  df_raw <- df_raw %>%
    clean_names() %>%
    mutate(response_id = str_pad(row_number(), width = 4, pad = "0")) %>%
    select(response_id, everything())

  ## Metadata
  yougov_meta <- c(
    "version", "exit_status", "respondent_status", "disposition", "last_page",
    "starttime", "endtime", "favor_or_oppose", "follow_up", "hcountry_code",
    "perc_skipped", "phone_flag", "points",
    ## I don't know what these are + must request browser types
    "posneg", "r1", "r2", "r3", "r4", "r5", "r6",
    "attention_check", "attention_check_word",
    "capabilities_touch", "invalidrank", "more"
  )

  ## Aggressively rename variables
  ## Next time, alert YouGov to preferred module/page names!
  temp <- df_raw %>%
    ## Make consistent with root_var
    ## Fix typos and inconsistencies in names
    rename_with(~ gsub("order", "_order", .x)) %>%
    rename_with(~ gsub("__", "_", .x)) %>%
    rename_with(~ gsub("polarization", "polar", .x)) %>%
    rename_with(~ gsub("identityhopkins", "anc_identity", .x)) %>%
    rename_with(~ gsub("holidays", "anc_polar", .x)) %>%
    rename_with(~ gsub("^alpha_", "anc_id_alphabet_", .x)) %>%
    rename_with(~ gsub("^exact_", "anc_id_exact_", .x)) %>%
    rename_with(~ gsub("^esystems_", "anc_esystems_", .x)) %>%
    rename_with(~ gsub("affectivepolar", "_polar", .x)) %>%
    rename_with(~ gsub("_1993", "", .x)) %>%
    rename_with(~ gsub("TernovskyScreener", "ternovski", .x)) %>%
    rename_with(~ gsub("apsa_", "esystems_", .x)) %>%
    rename_with(~ gsub("ternovsky_screener2", "ternovski", .x)) %>%
    rename_with(~ gsub("berinskyscreener", "berinsky", .x)) %>%
    rename_with(~ gsub("estemporal", "anc_es_temporal", .x)) %>%
    rename_with(~ gsub("esalphabet", "anc_es_alphabet", .x)) %>%
    rename_with(~ gsub("esystems_app", "app_esystems", .x)) %>%
    rename_with(~ gsub("anc_app_esystems", "app_esystems", .x)) %>%
    ## ordering variables
    rename_with(~ gsub("hopkins_order", "identity_order", .x)) %>%
    rename_with(~ gsub("app_order", "tate_order", .x))

  ## Print raw data's number of rows
  message(paste0("We have total ", nrow(temp), " respondents."))

  ## Make sure that there are no variations in the _module_rnd
  temp_rnd <- temp %>%
    select(
      ends_with("_module_rnd"),
      ends_with("_page_rnd"),
      ends_with("_col_rnd"),
      ends_with("order_q_rnd")
    )

  for (v in names(temp_rnd)) {
    ## After NAs are dismissed, there should be only one entry
    assert_that(length(setdiff(unique(temp_rnd[[v]]), c(NA, "NA"))) < 2)
  }

  ## Further zero-variance variables
  temp2 <- Filter(
    function(x) {
      case_when(
        all(is.na(x)) ~ FALSE,
        !any(is.na(x)) & var(x, na.rm = TRUE) == 0 ~ FALSE,
        TRUE ~ TRUE
      )
    },
    temp
  )

  setdiff(names(temp), names(temp2))
  #  [1] "consent"                               "invalidrank"
  #  [3] "page_esystems_order_q_timing"          "page_back_timing"
  #  [5] "page_birthyr_timing"                   "page_check_scores_timing"
  #  [7] "page_educ4_timing"                     "page_gender_timing"
  #  [9] "page_headers_timing"                   "page_identity_order_q_timing"
  # [11] "page_immigrant_screen_out_page_timing" "page_mobilescreenpage_timing"
  # [13] "page_p_empty_timing"                   "page_polar_order_q_timing"
  # [15] "page_race4_timing"                     "page_rand_1_ranking_timing"
  # [17] "page_rand_2_ranking_timing"            "page_rand_3_ranking_timing"
  # [19] "page_rand_4_ranking_timing"            "page_rand_5_ranking_timing"
  # [21] "page_region_timing"                    "page_tate_order_q_timing"
  # [23] "page_three_strikes_timing"             "page_votereg2_timing"
  # [25] "page_welcomeset_timing"                "phone_flag"

  ## Separate out timing variables away from substantial responses
  timing <- temp2 %>%
    select(
      response_id,
      matches(yougov_meta %>% paste(collapse = "|")),
      ## Timing
      ends_with("_timing"),
      ## No module-level, column, or page randomization took place
      ## Randomization was actually recorded in variables such as tate_order_q
      # ends_with("_module_rnd"),
      # ends_with("_page_rnd"),
      # ends_with("_col_rnd"),
      ## This is also empty
      # ends_width("order_q_rnd"),
      ## This is the important bit for robustness checks
      ends_with("_order_q"),
      ends_with("berinsky_rnd"),
      ## This indicates whether they were exposed to no-context 3 options vs. 4
      ## and such; it's pretty straightforward from NA values
      ## and besides, the naming and values are very uninformative
      # ends_with("_ranking")
      ## I'm not even sure what these are
      ## rand_5_ranking_rnd for example was randomized
      # starts_with("p_implicit_page_")
      # ends_with("_ranking_rnd)
      starts_with("rand_fact"),
      ends_with("_order"),
      ## Whether Dem was presented first or Rep
      matches("^dem_rep_can$"),
      matches("^lib_con$")
    )

  if ("starttime" %in% names(timing)) {
    timing <- timing %>%
      mutate(duration_in_minutes = endtime - starttime)
  } else {
    timing <- timing %>%
      mutate(duration_in_minutes = as.numeric(ms(endtime)) / 60)
  }

  main <- temp2 %>%
    select(
      !ends_with("_rnd"),
      ends_with("_row_rnd")
    ) %>%
    select(
      -ends_with("_timing"),
      # -ends_with("_module_rnd"),
      # -ends_with("_page_rnd"),
      # -ends_with("_col_rnd"),
      # -ends_with("berinsky_rnd"),
      # -ends_with("_ranking_rnd"),
      -ends_with("_order_q"),
      -ends_with("_ranking"),
      ## Otherwise, will kick gender3
      -matches(setdiff(yougov_meta, "r3") %>% paste(collapse = "|")),
      -starts_with("p_implicit_page_"),
      -starts_with("rand_fact"),
      -ends_with("_order"),
      -matches("^dem_rep_can$"),
      -matches("^lib_con$")
    )

  ## Delete further unnecessary variables (recheck with real data)
  main <- main %>%
    ## age and age4 already exist
    select(-contains("birthyr")) %>%
    ## gender3 is sufficient
    select(-contains("gender3_other"), -matches("^gender$")) %>%
    ## pid3 and pid7 is sufficient
    select(-pid3_t, -matches("^pid3_why$"), -pid7others, -pid7text) %>%
    ## Other text variables
    select(
      -matches("^religpew_t$"),
      -matches("^presvote20post_t$"),
      -matches("^housevote22post_t$")
    ) %>%
    ## hobbies/socialize are not substantial questions but cushions
    select(-contains("hobbies"), -contains("socialize"))

  ## Berinsky and Ternovski screeners
  if (any(grepl("selected", main$berinsky_1))) {
    main <- main %>%
      mutate(
        across(
          matches("berinsky|ternovski"),
          ~ case_when(
            .x == "selected" ~ 1,
            .x == "not selected" ~ 0,
            TRUE ~ NA_real_
          )
        )
      )
  }

  ## Unite columns
  main <- main %>%
    unite(
      col = "ternovski", sep = "|",
      ternovski_1, ternovski_2, ternovski_3, ternovski_4, ternovski_5
    )

  ## Unite rankings into a permutation pattern, a single variable
  main <- main %>%
    unite_ranking()

  ## Turn _row_rnd (Qualtrics equivalent of _do) into something more legible
  var_list <- main %>%
    select(ends_with("_row_rnd")) %>%
    names()

  ## YouGov has a particular style of showing randomized item order
  ## Make it consistent with existing code tailored to Qualtrics
  ## e.g., "randomize(5, [2,0,1])" ---> 312
  ## chatGPT output, spot checked
  item_order_transform <- function(input_string) {
    ## Extract substring between square brackets
    substring <- gsub(".*\\[([^]]+)\\].*", "\\1", input_string)

    ## Split the substring by comma
    numbers <- strsplit(substring, ",")[[1]]

    ## Increment each number by 1, remove spaces, and concatenate
    result <- ""
    for (number in numbers) {
      incremented_number <- as.numeric(trimws(number)) + 1
      result <- paste0(result, incremented_number)
    }

    ## Return NA if string NA
    if (result == "NA") {
      result <- NA
    }

    ## Return the final string
    return(result)
  }

  ## Transform the item order variables
  ## rowwise will slow things down; must find way to optimize
  message("Stripping item orders of brackets and other unnecessary strings.")
  main <- main %>%
    rowwise() %>%
    mutate(across(ends_with("_row_rnd"), ~ item_order_transform(.x))) %>%
    ungroup()

  ## Remember, we still don't have the *observed* ranking of the respondent
  ## unless the randomization has been given a 1234... natural order

  ## So if the true order respondent gave out  is 3-2-1
  ## but the order provided is                    3-1-2
  ## the *observed* ranking is                    1-3-2
  ## unite_ranking() will give 321 for this respondent.

  ## Simiarly, if the true order respondent gave out is 6-3-2-7-1-4-5
  ## but the order provided is                          6-1-5-2-7-3-4
  ## the true order respondent provided is              4-6-1-3-5-2-7

  ## Recover recorded rankings
  message("Recovering recorded rankings.")
  for (v in var_list) {
    main <- recover_recorded_responses(
      presented_order = v,
      true_order = gsub("_row_rnd", "", v),
      df = main
    )
  }

  ## Create binary indicators for anchor question/attention check/repeat q fails
  main <- main %>%
    ## 1 indicates failure; 0 indicates passing the test
    mutate(
      ## Attention checks
      ternovski_fail = case_when(
        ## Test link and actual data are different
        (ternovski != "1|1|0|0|0") & (ternovski != "1|1|2|2|2") ~ 1,
        (ternovski == "1|1|0|0|0") | (ternovski == "1|1|2|2|2") ~ 0
      ),
      ternovski_fail_label = case_when(
        (ternovski != "1|1|0|0|0") & (ternovski != "1|1|2|2|2") ~ "Failed",
        (ternovski == "1|1|0|0|0") | (ternovski == "1|1|2|2|2") ~ "Passed"
      ),
      berinsky_fail = case_when(
        (berinsky != "000100000001000000") &
          (berinsky != "222122222221222222") ~ 1,
        (berinsky == "000100000001000000") |
          (berinsky == "222122222221222222") ~ 0
      ),
      berinsky_fail_label = case_when(
        (berinsky != "000100000001000000") &
          (berinsky != "222122222221222222") ~ "Failed",
        (berinsky == "000100000001000000") |
          (berinsky == "222122222221222222") ~ "Passed"
      ),
      ## Random responses
      random_tate = case_when(
        anc_tate != "123" ~ 1,
        anc_tate == "123" ~ 0
      ),
      random_identity = case_when(
        anc_identity != "1234" ~ 1,
        anc_identity == "1234" ~ 0
      ),
      random_id_alphabet = case_when(
        anc_id_alphabet != "1234" ~ 1,
        anc_id_alphabet == "1234" ~ 0
      ),
      random_id_exact = case_when(
        ## The reason this particular string isn't 1234 is because
        ## teacher and relative were entered in the wrong order in the fielding
        anc_id_exact != "1243" ~ 1,
        anc_id_exact == "1243" ~ 0
      ),
      random_polar = case_when(
        anc_polar != "12345" ~ 1,
        anc_polar == "12345" ~ 0
      ),
      random_esystems = case_when(
        anc_esystems != "123456" ~ 1,
        anc_esystems == "123456" ~ 0
      ),
      random_es_alphabet = case_when(
        anc_es_alphabet != "123456" ~ 1,
        anc_es_alphabet == "123456" ~ 0
      ),
      random_es_temporal = case_when(
        anc_es_temporal != "123456" ~ 1,
        anc_es_temporal == "123456" ~ 0
      ),
      ## Perfectly collinear variables, just renamed/recoded for easy access
      anc_correct_tate = 1 - random_tate,
      anc_correct_identity = 1 - random_identity,
      anc_correct_id_alphabet = 1 - random_id_alphabet,
      anc_correct_id_exact = 1 - random_id_exact,
      anc_correct_polar = 1 - random_polar,
      anc_correct_esystems = 1 - random_esystems,
      anc_correct_es_alphabet = 1 - random_es_alphabet,
      anc_correct_es_temporal = 1 - random_es_temporal,
      ## Repeat questions
      repeat_tate = case_when(
        app_tate == app_tate_repeat ~ 0,
        app_tate != app_tate_repeat ~ 1
      ),
      repeat_identity = case_when(
        app_identity == app_identity_repeat ~ 0,
        app_identity != app_identity_repeat ~ 1
      ),
      repeat_polar = case_when(
        app_polar == app_polar_repeat ~ 0,
        app_polar != app_polar_repeat ~ 1
      ),
      repeat_esystems = case_when(
        app_esystems == app_esystems_repeat ~ 0,
        app_esystems != app_esystems_repeat ~ 1
      )
      ## Truncated rankings not yet propagated to main coding
    )

  ## Other wranglings
  main <- main %>%
    mutate(
      pid3final = case_when(
        pid7 %in% c(1, 2, 3) ~ "Democrat",
        pid7 %in% c(5, 6, 7) ~ "Republican",
        pid7 %in% c(4, 8) ~ "Independent"
      ),
      partisan = case_when(
        pid7 %in% c(1, 7) ~ "Strong Partisan",
        pid7 %in% c(2, 3, 5, 6) ~ "Weak Partisan",
        pid7 %in% c(4, 8) ~ "Independent"
      ),
      race4labeled = case_when(
        race4 == 1 ~ "White",
        race4 == 2 ~ "Black",
        race4 == 3 ~ "Hispanic",
        race4 == 4 ~ "Other race"
      ),
      race4asians = case_when(
        race == 1 ~ "White",
        race == 2 ~ "Black",
        race == 3 ~ "Hispanic",
        race == 4 ~ "Asian"
      ),
      race2 = case_when(
        race4 == 1 ~ "White",
        race4 %in% c(2, 3, 4) ~ "None-white"
      ),
      male = ifelse(gender3 == 1, 1, 0)
    )

  ## Only complete responses
  if ("respondent_status" %in% names(timing)) {
    timing <- timing %>%
      filter(respondent_status == "Complete")
    main <- main %>%
      filter(response_id %in% timing$response_id)
  }

  ## After filtering
  message(paste0("We have total ", nrow(main), " respondents after filtering."))

  return(list(main = main, timing = timing, raw = df_raw))
}
