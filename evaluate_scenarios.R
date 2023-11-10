###
# Evaluation of scenario experiments
#

source("setup.R")
source("model_features.R")

#install.packages("xtable")
library(xtable)

models <- list(
  aggregates_model,
  utilities_model,
  bagofcards_model,
  ranking_model
)

# alle scenario experiments are executed with this sample size
TRAIN_LOG_SIZE <- 20000

read_log_matches_filtered <- function(decks_filename, log_filename, n=-1) {
  decks <- lapply(strsplit(readLines(decks_filename, warn=FALSE), ",", fixed=TRUE), as.numeric)
  log <- read_log_matches(log_filename, n)
  log_filtered <- log %>% filter(deck %in% decks)
  return(log_filtered)
}

read_test_data <- function(filename) {
  test_data <- read.csv2(filename)
  test_data$deck <- lapply(test_data$deck, function(d) eval(parse(text=d)))
  return(test_data)
}

read_decks <- function(filename) {
  decks <- lapply(strsplit(readLines(filename, warn=FALSE), ",", fixed=TRUE), as.numeric)
  return(decks)
}

debug_build_model <- function(train_cardlist_file, log_train, model_definition, train_size=TRAIN_LOG_SIZE) {
  indices <- sample(1:nrow(log_train), train_size)
  train_data <- prepare_log(log_train[indices, ], model_definition$feature_fn(read_cardlist(train_cardlist_file)))
  m <- model_definition$model_fn(train_data)
  return(m)
}

evaluate_spearman <- function(train_cardlist_file, log_train, test_cardlist_file, test_data, model_definitions, train_size=TRAIN_LOG_SIZE, repetitions=10) {
  df <- bind_rows(apply(expand.grid(model=model_definitions, repetition=1:repetitions), 1, function(args) {
    model_definition <- args$model
    indices <- sample(1:nrow(log_train), train_size)
    train_data <- prepare_log(log_train[indices, ], model_definition$feature_fn(read_cardlist(train_cardlist_file)))
    test_features <- prepare_test_data(test_data$deck, model_definition$feature_fn(read_cardlist(test_cardlist_file)))
    
    m <- model_definition$model_fn(train_data)
    predicted_scores <- get_scores(m, test_features)
    spearman_cor <- cor(test_data$win_rate, predicted_scores, method="spearman")
    return(data.frame(name=model_definition$name, metric=spearman_cor))
  })) %>%
    group_by(name) %>%
    summarise(
      spearman_cor=mean(metric),
      spearman_cor_sd=sd(metric),
      spearman_cor_min=min(metric),
      spearman_cor_max=max(metric),
      spearman_cor_lower=quantile(metric, 0.025),
      spearman_cor_upper=quantile(metric, 0.975)
    )
  
  return(df)
}

run_bootstrap_splits <- function(eval_fn, a_size, b_size, train_size=TRAIN_LOG_SIZE, fractions=seq(0.0, 1.0, 0.05), repetitions=10, bootstrap_until=1000, summarize=TRUE) {
  run_eval <- function(fraction, repetition) {
    num_a <- (1 - fraction) * train_size
    # replace old entries instead of appending them so that no model can benefit from the larger training sample
    num_b <- fraction * train_size
    
    if (min(num_a, num_b) > bootstrap_until && repetition > 1) {
      # sample is big enough, skip run
      return(data.frame(fraction=fraction, metric=NA))
    }
    
    if (num_a > 0) {
      a_indices <- sample(1:a_size, num_a, replace=TRUE)
    } else {
      a_indices <- c()
    }
    
    if (num_b > 0) {
      b_indices <- sample(1:b_size, num_b, replace=TRUE)
    } else {
      b_indices <- c()
    }
    
    metric <- eval_fn(a_indices, b_indices)
    return(data.frame(metric=metric, fraction=fraction))
  }
  
  df <- bind_rows(apply(expand.grid(fraction=fractions, repetition=1:repetitions), 1, function(args) run_eval(args[[1]], args[[2]])))
  
  if (summarize) {
    df <- df %>%
      filter(!is.na(metric)) %>%
      group_by(fraction) %>%
      summarize(metric_mean=mean(metric), metric_sd=sd(metric))
  }
  
  return(df)
}

calculate_card_statistics <- function(log, cardlist) {
  # correlations of attributes
  stats_by_card <- log %>%
    unnest(cols=deck) %>%
    group_by(deck) %>%
    summarise(win_rate = sum(victory)/length(victory)) %>%
    inner_join(cardlist %>% mutate(id = as.numeric(rownames(cardlist))), by = c("deck" = "id")) %>%
    mutate(card=deck)
  
  corrs <- stats_by_card %>%
    group_by(type) %>%
    summarise(across(
      c("attack", "defense", "cost", "ability_b", "ability_c", "ability_d", "ability_g", "ability_l", "ability_w", "playerHP", "enemyHP", "cardDraw"),
      function(stat) {
        if (sd(stat) == 0) {
          return(0)
        }
        corr <- cor(win_rate, stat)
        if (is.na(corr)) {
          return(0)
        } else {
          return(corr)
        }
      }
    )) %>%
    pivot_longer(!type, names_to="statistic", values_to="correlation") %>%
    arrange(-abs(correlation))
  
  return(list(corrs=corrs, stats_by_card=stats_by_card))
}

calibration_curves <- function(train_cardlist_file, log_train, test_cardlist_file, test_data, model_definitions, train_size=TRAIN_LOG_SIZE, repetitions=10) {
  df <- bind_rows(apply(expand.grid(model=model_definitions, repetition=1:repetitions), 1, function(args) {
    model_definition <- args$model
    indices <- sample(1:nrow(log_train), train_size)
    train_data <- prepare_log(log_train[indices, ], model_definition$feature_fn(read_cardlist(train_cardlist_file)))
    test_features <- prepare_test_data(test_data$deck, model_definition$feature_fn(read_cardlist(test_cardlist_file)))
    
    m <- model_definition$model_fn(train_data)
    predicted_scores <- get_scores(m, test_features)
    return(data.frame(name=model_definition$name, expected=test_data$win_rate, actual=predicted_scores))
  })) %>%
    group_by(name, expected) %>%
    summarise(actual=mean(actual))
  
  p <- df %>%
    ggplot(aes(x=actual, y=expected, color=name)) + geom_point() +
    geom_abline(intercept=0, slope=1) +
    geom_smooth(method="lm", se=FALSE, color="black") +
    facet_grid(name ~ .) +
    labs(x="Geschätzte Siegesquote", y="Empirische Siegesquote") +
    lims(x=c(0.2, 0.8), y=c(0.2, 0.8)) +
    theme(legend.position="none")
  
  return(list(df=df, p=p))
}

###
# Scenario 1
#

train_data_scenario1 <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log.njson")
test_data_scenario1 <- read_test_data("logs/test_decks_results.csv")

# EDA - Density Plot of win rates
test_data_scenario1 %>%
  ggplot(aes(x=win_rate)) + geom_histogram(binwidth=0.05) +
  labs(x="Win Rate Bucket", y="Count") + xlim(c(0.0, 1.0))
ggsave("./plots/scenario1_density.eps", device="eps", width=8, height=6, units="cm", dpi="print")

mean(test_data_scenario1$win_rate)
mean(test_data_scenario1$sd_win_rate)

# Calibration
calibration_1 <- calibration_curves("cardlist.txt", train_data_scenario1, "cardlist.txt", test_data_scenario1, model_definitions=models, repetitions=1)
# repetitions=1, no advantage from bootstrapping
calibration_1$p
ggsave("./plots/scenario1_calibration.eps", device="eps", width=12, height=12, units="cm", dpi="print")

# Correlation
corrs_1 <- evaluate_spearman("cardlist.txt", train_data_scenario1, "logs/cardlist.txt", test_data_scenario1, model_definitions=models)

corrs_1 %>%
  mutate(spearman_ci=sprintf("[%.4f, %.4f]", spearman_cor_lower, spearman_cor_upper)) %>%
  select(name, spearman_cor, spearman_ci) %>%
  xtable(digits=4) %>%
  print(include.rownames=FALSE)

evaluate_scenario_1 <- function(model_definitions) {
  cardlist <- read_cardlist()
  log_train <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log.njson")
  test_data <- read_test_data("logs/test_decks_results.csv")
  
  train_size <- nrow(log_train)
  steps <- 100
  train_fractions <- ((1:sqrt(train_size))**2 / train_size)[seq(0, sqrt(train_size), sqrt(train_size)/steps)]
  
  df <- bind_rows(lapply(model_definitions, function(model_definition) {
    print(model_definition$name)
  
    feature_fn_applied <- model_definition$feature_fn(cardlist)
    train_data <- prepare_log(log_train, feature_fn_applied)
    test_features <- prepare_test_data(test_data$deck, feature_fn_applied)
    
    eval_fn <- function(unused, slice_indices) {
      tryCatch({
        train_data_slice <- slice_train_data(train_data, slice_indices)
        m <- model_definition$model_fn(train_data_slice)
        predicted_scores <- get_scores(m, test_features)
        spearman_cor <- cor(predicted_scores, test_data$win_rate, method="spearman")
        return(spearman_cor)
      }, error=function(cond) {
        print(cond)
        return(NA)
      })
    }
    
    df <- run_bootstrap_splits(eval_fn, 0, length(train_data$target), train_size=length(train_data$target), fractions=train_fractions)
    df$name <- model_definition$name
    return(df)
  }))
  
  p <- df %>%
    mutate(entries=floor(train_size*fraction)) %>%
    ggplot(aes(y=metric_mean, x=entries, color=name)) +
    theme(legend.position="bottom") +
    labs(x="Number of log entries", y="Spearman Correlation", color="Model") +
    scale_x_continuous(trans="sqrt", breaks=c(1000, 10000, 25000, 50000, 100000, 150000)) +
    geom_point()
  
  ggsave("./plots/scenario1.eps", plot=p, device="eps", width=12, height=9, units="cm", dpi="print")
  
  return(list(p=p, df=df))
}

res1 <- evaluate_scenario_1(model_definitions=models)


###
# Scenario 2
#

train_data_scenario_2 <- read_log_matches_filtered("logs/train_decks_new_card.txt", "logs/train_log_new_card_2.njson")

# How strongly do win rates in the train dataset correlatio with those of V1?
cardlist_1 <- read_cardlist()
card_stats_1 <- calculate_card_statistics(train_data_scenario1, cardlist_1)

cardlist_2 <- read_cardlist("logs/cardlist_new_card.txt")
card_stats_2 <- calculate_card_statistics(train_data_scenario_2, cardlist_2)

cor(card_stats_1$stats_by_card$win_rate, card_stats_2$stats_by_card$win_rate[1:160])

# EDA

# Win Rate before
train_data_scenario1 %>%
  summarise(picks=length(victory), wins=sum(victory), win_rate=wins/picks)

# Win Rate overall
train_data_scenario_2 %>%
  summarise(picks=length(victory), wins=sum(victory), win_rate=wins/picks)

# best win rate of old cards
train_data_scenario1 %>%
  unnest(cols=deck) %>%
  group_by(card=deck) %>%
  summarise(picks=length(victory), wins=sum(victory), win_rate=wins/picks) %>%
  arrange(-win_rate) %>%
  head(n=1) %>%
  select(card, win_rate) %>%
  pull

# win rates of new cards
train_data_scenario_2 %>%
  unnest(cols=deck) %>%
  group_by(card=deck) %>%
  summarise(picks=length(victory), wins=sum(victory), win_rate=wins/picks) %>%
  filter(card >= 161) %>%
  arrange(-win_rate) %>%
  select(card, win_rate) %>%
  xtable(digits=5) %>%
  print(include.rownames=FALSE)

test_data_scenario_2 <- read_test_data("logs/test_decks_results_new_card_biased_2.csv")

# Density Plot of win rates
test_data_scenario_2 %>%
  ggplot(aes(x=win_rate)) + geom_histogram(binwidth=0.05) +
  labs(x="Win Rate Bucket", y="Count") + xlim(c(0.0, 1.0))
ggsave("./plots/scenario2_density.eps", device="eps", width=8, height=6, units="cm", dpi="print")

mean(test_data_scenario_2$win_rate)
mean(test_data_scenario_2$sd_win_rate)

# Calibration of old training data and new test data
calibration_2 <- calibration_curves("cardlist.txt", train_data_scenario1, "logs/cardlist_new_card.txt", test_data_scenario_2, model_definitions=models, repetitions=1)
calibration_2$p
ggsave("./plots/scenario2_calibration.eps", device="eps", width=7.5, height=10, units="cm", dpi="print")

# Correlation with old training data and new test data
corrs_2 <- evaluate_spearman("cardlist.txt", train_data_scenario1, "logs/cardlist_new_card.txt", test_data_scenario_2, model_definitions=models)

corrs_2 %>%
  mutate(spearman_ci=sprintf("[%.4f, %.4f]", spearman_cor_lower, spearman_cor_upper)) %>%
  select(name, spearman_cor, spearman_ci) %>%
  xtable(digits=4) %>%
  print(include.rownames=FALSE)

evaluate_scenario_2 <- function(model_definitions) {
  cardlist_old <- read_cardlist()
  log_train_old <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log.njson")

  cardlist_new <- read_cardlist("logs/cardlist_new_card.txt")
  log_train_new <- read_log_matches_filtered("logs/train_decks_new_card.txt", "logs/train_log_new_card_2.njson")

  test_data <- read_test_data("logs/test_decks_results_new_card_biased_2.csv")
    
  df <- bind_rows(lapply(model_definitions, function(model_definition) {
    print(model_definition$name)
    
    train_data_old <- prepare_log(log_train_old, model_definition$feature_fn(cardlist_old))
    feature_fn_applied_new <- model_definition$feature_fn(cardlist_new)
    train_data_new <- prepare_log(log_train_new, feature_fn_applied_new)
    test_features <- prepare_test_data(test_data$deck, feature_fn_applied_new)
    
    eval_fn <- function(old_slice_indices, new_slice_indices) {
      tryCatch({
        train_data_slice <- concat_train_data(
          slice_train_data(train_data_old, old_slice_indices),
          slice_train_data(train_data_new, new_slice_indices)
        )
        m <- model_definition$model_fn(train_data_slice)
        predicted_scores <- get_scores(m, test_features)
        spearman_cor <- cor(predicted_scores, test_data$win_rate, method="spearman")
        return(spearman_cor)
      }, error=function(cond) {
        print(cond)
        return(NA)
      })
    }
    
    df <- run_bootstrap_splits(eval_fn, length(train_data_old$target), length(train_data_new$target), bootstrap_until = 9999999)
    df$name <- model_definition$name
    return(df)
  }))
  
  p <- df %>%
    ggplot(aes(y=metric_mean, x=fraction, color=name)) +
    theme(legend.position="bottom") +
    labs(x="Number of new log entries", y="Spearman Correlation", color="Model") +
    ylim(-0.05, 0.65) +
    geom_point()
  
  ggsave("./plots/scenario2.eps", plot=p, device="eps", width=12, height=9, units="cm", dpi="print")

  return(list(df=df, p=p))
}

res2 <- evaluate_scenario_2(model_definitions=models) # diagram is very noisy without bootstrapping

res2$p + labs(x="fraction of new logs in training data", y="correlation", color="RS") + ylim(-0.05, 0.65)
ggsave("./plots/scenario2.eps", device="eps", width=12, height=9, units="cm", dpi="print")

# calibration with new training data
calibration_2_after <- calibration_curves("logs/cardlist_new_card.txt", train_data_scenario_2, "logs/cardlist_new_card.txt", test_data_scenario_2, model_definitions=models, repetitions=1)
calibration_2_after$p
ggsave("./plots/scenario2_calibration_after.eps", device="eps", width=7.5, height=10, units="cm", dpi="print")

# correlation with new training data
corrs_2_after <- evaluate_spearman("logs/cardlist_new_card.txt", train_data_scenario_2, "logs/cardlist_new_card.txt", test_data_scenario_2, model_definitions=models)

corrs_2_after %>%
  mutate(spearman_ci=sprintf("[%.4f, %.4f]", spearman_cor_lower, spearman_cor_upper)) %>%
  select(name, spearman_cor, spearman_ci) %>%
  xtable(digits=4) %>%
  print(include.rownames=FALSE)

###
# Scenario 3 - Balancing
#

# Correlation of attributes before
card_corrs_1 <- card_stats_1$corrs
card_corrs_1 %>%
  head(n=15) %>%
  xtable(digits=3) %>%
  print(include.rownames=FALSE)

train_data_scenario_3 <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log_balance.njson")
test_data_scenario_3_biased <- read_test_data("logs/test_decks_results_balance_biased.csv")

# Correlation of attributes after
cardlist_3 <- read_cardlist("./logs/cardlist_balance.txt")
card_stats_3 <- calculate_card_statistics(train_data_scenario_3, cardlist_3)

inner_join(card_stats_1$corrs, card_stats_3$corrs, by=join_by(type, statistic)) %>%
  head(n=15) %>%
  xtable(digits=3) %>%
  print(include.rownames=FALSE)

buffed_cards <- c(57, 140, 110, 108, 7, 127)
nerfed_cards <- c(18, 123, 124, 69, 83, 21)
balanced_cards <- c(buffed_cards, nerfed_cards)

# How big is the impact of the changes?
cor(card_stats_1$stats_by_card$win_rate, card_stats_3$stats_by_card$win_rate)

# Win Rates of changed cards
data.frame(
  id=balanced_cards,
  win_rate_before=card_stats_1$stats_by_card[balanced_cards, "win_rate"],
  win_rate_after=card_stats_3$stats_by_card[balanced_cards, "win_rate"]
) %>%
  xtable(digits=c(0, 0, 4, 4)) %>%
  print(include.rownames=FALSE)

# Correlation with old training data and new test data
corrs_3 <- evaluate_spearman("cardlist.txt", train_data_scenario1, "logs/cardlist_balance.txt", test_data_scenario_3_biased, model_definitions=models)

corrs_3 %>%
  mutate(spearman_ci=sprintf("[%.4f, %.4f]", spearman_cor_lower, spearman_cor_upper)) %>%
  select(name, spearman_cor, spearman_ci) %>%
  xtable(digits=4) %>%
  print(include.rownames=FALSE)

# Density Plot of win rates
test_data_scenario_3_biased %>%
  ggplot(aes(x=win_rate)) + geom_histogram(binwidth=0.05) +
  labs(x="Win Rate Bucket", y="Count") + xlim(c(0.0, 1.0))
ggsave("./plots/scenario3_density.eps", device="eps", width=8, height=6, units="cm", dpi="print")

mean(test_data_scenario_3_biased$win_rate)
mean(test_data_scenario_3_biased$sd_win_rate)

# Calibration with old training data and new test data
calibration_3 <- calibration_curves("cardlist.txt", train_data_scenario1, "logs/cardlist_balance.txt", test_data_scenario_3_biased, model_definitions=models, repetitions=1)
calibration_3$p
ggsave("./plots/scenario3_calibration.eps", device="eps", width=7.5, height=10, units="cm", dpi="print")

evaluate_scenario_3 <- function(name, model_definitions) {
  cardlist_old <- read_cardlist()
  log_train_old <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log.njson")
  
  cardlist_new <- read_cardlist("logs/cardlist_balance.txt")
  log_train_new <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log_balance.njson")
  
  test_data <- read_test_data("logs/test_decks_results_balance_biased.csv")
  
  df <- bind_rows(lapply(model_definitions, function(model_definition) {
    print(model_definition$name)
    
    train_data_old <- prepare_log(log_train_old, model_definition$feature_fn(cardlist_old))
    feature_fn_applied_new <- model_definition$feature_fn(cardlist_new)
    train_data_new <- prepare_log(log_train_new, feature_fn_applied_new)
    test_features <- prepare_test_data(test_data$deck, feature_fn_applied_new)
    
    eval_fn <- function(old_slice_indices, new_slice_indices) {
      tryCatch({
        train_data_slice <- concat_train_data(
          slice_train_data(train_data_old, old_slice_indices),
          slice_train_data(train_data_new, new_slice_indices)
        )
        m <- model_definition$model_fn(train_data_slice)
        predicted_scores <- get_scores(m, test_features)
        spearman_cor <- cor(predicted_scores, test_data$win_rate, method="spearman")
        return(spearman_cor)
      }, error=function(cond) {
        print(cond)
        return(NA)
      })
    }
    
    df <- run_bootstrap_splits(eval_fn, length(train_data_old$target), length(train_data_new$target))
    df$name <- model_definition$name
    return(df)
  }))
  
  p <- df %>%
    ggplot(aes(y=metric_mean, x=fraction, color=name)) +
    theme(legend.position="bottom") +
    labs(x="Fraction of new log entries", y="Spearman Correlation", color="Model") +
    ylim(-0.05, 0.65) +
    geom_point()
  
  ggsave("./plots/scenario3.eps", plot=p, device="eps", width=12, height=9, units="cm", dpi="print")
  
  return(list(df=df, p=p))
}

res3 <- evaluate_scenario_3(model_definitions=models)

res3$p + labs(x="fraction of new logs in training data", y="correlation", color="RS") + ylim(-0.05, 0.65)
ggsave("./plots/scenario3.eps", device="eps", width=12, height=9, units="cm", dpi="print")

# Calibraiton with new training data
calibration_3_after <- calibration_curves("logs/cardlist_balance.txt", train_data_scenario_3, "logs/cardlist_balance.txt", test_data_scenario_3_biased, model_definitions=models, repetitions=1)
calibration_3_after$p
ggsave("./plots/scenario3_calibration_after.eps", device="eps", width=7.5, height=10, units="cm", dpi="print")

# Correlation with new training data and old test data
corrs_3 <- evaluate_spearman("logs/cardlist_balance.txt", train_data_scenario_3, "logs/cardlist_balance.txt", test_data_scenario_3_biased, model_definitions=models)

corrs_3 %>%
  mutate(spearman_ci=sprintf("[%.4f, %.4f]", spearman_cor_lower, spearman_cor_upper)) %>%
  select(name, spearman_cor, spearman_ci) %>%
  xtable(digits=4) %>%
  print(include.rownames=FALSE)

###
# Scenario 4
#

# How impactful is the change? Correlation with old training data
train_data_scenario_4 <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log_trend.njson")
card_stats_4 <- calculate_card_statistics(train_data_scenario_4, cardlist_1)
cor(card_stats_1$stats_by_card$win_rate, card_stats_4$stats_by_card$win_rate)

# more experiments
train_data_scenario_4b <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log_trend_2.njson")
card_stats_4b <- calculate_card_statistics(train_data_scenario_4b, cardlist_1)
cor(card_stats_1$stats_by_card$win_rate, card_stats_4b$stats_by_card$win_rate)

train_data_scenario_4c <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log_trend_3.njson")
card_stats_4c <- calculate_card_statistics(train_data_scenario_4c, cardlist_1)
cor(card_stats_1$stats_by_card$win_rate, card_stats_4c$stats_by_card$win_rate)

# Correlation with old training data and new test data
test_data_scenario_4 <- read_test_data("logs/test_decks_results_trend_biased.csv")
corrs_4 <- evaluate_spearman("cardlist.txt", train_data_scenario1, "logs/cardlist.txt", test_data_scenario_4, model_definitions=models)

corrs_4 %>%
  mutate(spearman_ci=sprintf("[%.4f, %.4f]", spearman_cor_lower, spearman_cor_upper)) %>%
  select(name, spearman_cor, spearman_ci) %>%
  xtable(digits=4) %>%
  print(include.rownames=FALSE)

# # Which cards' win rates are affected the most?
# most_impacted_cards <- inner_join(card_stats_1$stats_by_card, card_stats_4$stats_by_card, by=c("deck"), suffix=c("", ".trend")) %>%
#   mutate(win_rate.diff = win_rate.trend-win_rate) %>%
#   arrange(-abs(win_rate.diff)) %>%
#   select(deck, type, win_rate.diff) %>%
#   head(10) %>%
#   pull(deck)
# most_impacted_cards[, 1] # used to generate test decks
# # replaced IDs in V2:
# # unname(sapply(cs$name[c(20, 43, 67, 69, 84, 123, 125, 128, 131, 136, 141, 149, 151, 155, 159)], function(n) which(cardlist_2$name %in% n)))

# Correlation of card staticstics - what is the impact of the change in opponent population?
card_stats_1$corrs %>%
  inner_join(card_stats_4$corrs, suffix=c(".before", ".after"), by=join_by(type, statistic)) %>%
  mutate(correlation.diff=correlation.after-correlation.before) %>%
  arrange(-abs(correlation.diff)) %>%
  head(n=15) %>%
  xtable(digits=3) %>%
  print(include.rownames=FALSE)

# Density Plot of win rates
test_data_scenario_4 %>%
  ggplot(aes(x=win_rate)) + geom_histogram(binwidth=0.05) +
  labs(x="Siegesquote-Bucket", y="Anzahl") + xlim(c(0.0, 1.0))
ggsave("./plots/scenario4_density.eps", device="eps", width=8, height=6, units="cm", dpi="print")

mean(test_data_scenario_4$win_rate)
mean(test_data_scenario_4$sd_win_rate)

# Calibraiton with old training data and new test data
calibration_4 <- calibration_curves("cardlist.txt", train_data_scenario1, "logs/cardlist.txt", test_data_scenario_4, model_definitions=models, repetitions=1)
calibration_4$p
ggsave("./plots/scenario4_calibration.eps", device="eps", width=7.5, height=10, units="cm", dpi="print")

evaluate_scenario_4 <- function(model_definitions) {
  cardlist <- read_cardlist()
  
  log_train_old <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log.njson")
  log_train_new <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log_trend.njson")
  test_data <- read_test_data("logs/test_decks_results_trend_biased.csv")
  
  df <- bind_rows(lapply(model_definitions, function(model_definition) {
    print(model_definition$name)
    
    feature_fn_applied <- model_definition$feature_fn(cardlist)
    train_data_old <- prepare_log(log_train_old, feature_fn_applied)
    train_data_new <- prepare_log(log_train_new, feature_fn_applied)
    test_features <- prepare_test_data(test_data$deck, feature_fn_applied)
    
    eval_fn <- function(old_slice_indices, new_slice_indices) {
      tryCatch({
        train_data_slice <- concat_train_data(
          slice_train_data(train_data_old, old_slice_indices),
          slice_train_data(train_data_new, new_slice_indices)
        )
        m <- model_definition$model_fn(train_data_slice)
        predicted_scores <- get_scores(m, test_features)
        spearman_cor <- cor(predicted_scores, test_data$win_rate, method="spearman")
        return(spearman_cor)
      }, error=function(cond) {
        print(cond)
        return(NA)
      })
    }
    
    df <- run_bootstrap_splits(eval_fn, length(train_data_old$target), length(train_data_new$target), bootstrap_until=999999)
    df$name <- model_definition$name
    return(df)
  }))
  
  p <- df %>%
    ggplot(aes(y=metric_mean, x=fraction, color=name)) +
    theme(legend.position="bottom") +
    labs(x="Fraction of new log entries", y="Spearman Correlation", color="Model") +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) # TODO entfernen?
  
  ggsave("./plots/scenario4.eps", plot=p, device="eps", width=12, height=9, units="cm", dpi="print")
  
  return(list(df=df, p=p))
}

res4 <- evaluate_scenario_4(model_definitions=models)

res4$df %>%
  ggplot(aes(y=metric_mean, x=fraction, color=name)) +
  theme(legend.position="bottom") +
  geom_point() +
  labs(x="fraction of new logs in training data", y="correlation", color="RS") +
  ylim(-0.05, 0.65)
ggsave("./plots/scenario4.eps", device="eps", width=12, height=9, units="cm", dpi="print")

calibration_4_after <- calibration_curves("logs/cardlist.txt", train_data_scenario_4, "logs/cardlist.txt", test_data_scenario_4, model_definitions=models, repetitions=1)
calibration_4_after$p
ggsave("./plots/scenario4_calibration_after.eps", device="eps", width=7.5, height=10, units="cm", dpi="print")

# Correlation with new training data and new test data
corrs_4 <- evaluate_spearman("logs/cardlist.txt", train_data_scenario_4, "logs/cardlist.txt", test_data_scenario_4, model_definitions=models)

corrs_4 %>%
  mutate(spearman_ci=sprintf("[%.4f, %.4f]", spearman_cor_lower, spearman_cor_upper)) %>%
  select(name, spearman_cor, spearman_ci) %>%
  xtable(digits=4) %>%
  print(include.rownames=FALSE)




evaluate_scenario_4_coefficients <- function(model_definitions) {
  cardlist <- read_cardlist()
  
  log_train_old <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log.njson")
  log_train_new <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log_trend.njson")
  test_data <- read_test_data("logs/test_decks_results_trend_biased.csv")
  
  df <- bind_rows(lapply(model_definitions, function(model_definition) {
    print(model_definition$name)
    
    feature_fn_applied <- model_definition$feature_fn(cardlist)
    train_data_old <- prepare_log(log_train_old, feature_fn_applied)
    train_data_new <- prepare_log(log_train_new, feature_fn_applied)
    test_features <- prepare_test_data(test_data$deck, feature_fn_applied)
    
    eval_fn <- function(old_slice_indices, new_slice_indices) {
      tryCatch({
        train_data_slice <- concat_train_data(
          slice_train_data(train_data_old, old_slice_indices),
          slice_train_data(train_data_new, new_slice_indices)
        )
        m <- model_definition$model_fn(train_data_slice)
        return(I(list(m$coefficients)))
      }, error=function(cond) {
        print(cond)
        return(NA)
      })
    }
    
    df <- run_bootstrap_splits(eval_fn, length(train_data_old$target), length(train_data_new$target), summarize=FALSE, bootstrap_until=9999999)
    df$name <- model_definition$name
    return(df)
  }))
  
  return(list(df=df))
}

# all models; 1 without bootstrap
e4_c <- evaluate_scenario_4_coefficients(models)

# Utilities; 10 bootstrap
e4_utilities_c <- evaluate_scenario_4_coefficients(list(utilities_model))

# Graph:
e4_utilities_c$df %>%
  filter(name=="Utility") %>%
  mutate(data=purrr::map(metric, unlist)) %>%
  unnest_wider(data) %>%
  filter(!is.na(synergy_score)) %>%
  group_by(fraction) %>%
  summarise(
    cost_effectiveness=mean(cost_effectiveness),
    synergy_score=mean(synergy_score),
    strategy_parameter=mean(strategy_parameter),
    mana=mean(mana)
  ) %>%
  ggplot(aes(x=fraction)) +
  geom_point(aes(y=cost_effectiveness, col="Cost Effectiveness")) +
  geom_point(aes(y=synergy_score, col="Synergy Score")) +
  geom_point(aes(y=strategy_parameter, col="Strategy Parameter")) +
  geom_point(aes(y=mana, col="Average Mana Cost")) +
  theme(legend.position="bottom", legend.margin=margin()) +
  labs(y="coefficient", x="fraction of new logs in training data", col="variable") + guides(col=guide_legend(nrow=2, byrow=TRUE))

ggsave("./plots/scenario4-coefficients.eps", device="eps", width=13, height=9.75, units="cm", dpi="print")


e4_c$df %>%
  filter(name=="Ranking") %>%
  mutate(data=purrr::map(metric, unlist)) %>%
  unnest_wider(data) %>%
  ggplot(aes(y=mean_rank, x=fraction)) + geom_point()


e4_c$df %>%
  filter(name=="Utility") %>%
  mutate(data=purrr::map(metric, unlist)) %>%
  unnest_wider(data) %>%
  ggplot(aes(y=synergy_score, x=fraction)) + geom_point()
# cost_effectiveness synergy_score, strategy_parameter, mana



e4_c$df %>%
  filter(name=="Utility") %>%
  mutate(data=purrr::map(metric, unlist)) %>%
  unnest_wider(data) %>%
  ggplot(aes(x=fraction)) +
  geom_point(aes(y=`(Intercept)`, col="Intercept")) +
  geom_point(aes(y=cost_effectiveness, col="Cost Effectiveness")) +
  geom_point(aes(y=synergy_score, col="Synergy Score")) +
  geom_point(aes(y=strategy_parameter, col="Strategy Parameter")) +
  geom_point(aes(y=mana, col="Average Mana Cost")) +
  labs(y="coefficient", col="variable")


e4_c$df %>%
  filter(name=="Aggregates") %>%
  mutate(data=purrr::map(metric, unlist)) %>%
  unnest_wider(data) %>%
  ggplot(aes(x=fraction)) +
  geom_point(aes(y=`(Intercept)`, col="Intercept")) +
  geom_point(aes(y=creature_attack_mean, col="creature_attack_mean")) +
  geom_point(aes(y=creature_defense_mean, col="creature_defense_mean")) +
  geom_point(aes(y=ability_g_count, col="ability_g_count")) +
  geom_point(aes(y=cost_mean, col="cost_mean")) +
  geom_point(aes(y=type_itemRed_count, col="type_itemRed_count")) +
  labs(y="coefficient", col="variable")



e4_c$df %>%
  filter(name=="Ranking") %>%
  mutate(data=purrr::map(metric, unlist)) %>%
  unnest_wider(data) %>%
  ggplot(aes(x=fraction)) +
  geom_point(aes(y=`(Intercept)`, col="Intercept")) +
  geom_point(aes(y=mean_rank, col="mean_rank")) +
  labs(y="coefficient", col="variable")

