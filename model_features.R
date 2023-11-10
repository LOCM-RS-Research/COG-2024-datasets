###
# Regression model definitions
#

#install.packages("rpart.plot", "moments")

prepare_log <- function(log, feature_fn) {
  features_df <- bind_rows(lapply(log$deck, feature_fn))
  target <- factor(log$victory, levels=c(FALSE, TRUE), labels=c("LOSS", "WIN"))
  return(list(features=features_df, target=target))
}

slice_train_data <- function(train_data, slice_indices) {
  train_data_slice <- list(
    features=train_data$features[slice_indices, , drop=FALSE],
    target=train_data$target[slice_indices]
  )
  return(train_data_slice)
}

concat_train_data <- function(train_data1, train_data2) {
  train_data <- list(
    features=bind_rows(train_data1$features, train_data2$features),
    target=c(train_data1$target, train_data2$target)
  )
  return(train_data)
}

prepare_test_data <- function(test_decks, feature_fn) {
  features_df <- bind_rows(lapply(test_decks, feature_fn))
  return(features_df)
}

get_scores <- function(model, test_data) {
  scores <- predict(model, test_data, type="response")
  return(scores)
}

local_search <- function(deck, num_cards, n, evaluation_fn) {
  solution <- deck
  
  for (i in 1:n) {
    # create matrix with all possible swaps
    swap_matrix <- expand.grid(card_to_remove = solution, card_to_add = setdiff(1:num_cards, solution))
    # optional: random search instead of exhaustive search
    #swap_matrix <- swap_matrix[sample(nrow(swap_matrix),size=100),]
    cards_to_evaluate <- t(apply(swap_matrix, 1, function(x) c(solution[solution != x[[1]]], x[[2]])))
    
    # evaluate all swaps
    fitness_values <- evaluation_fn(cards_to_evaluate) # matrix, rows: decks
    
    best_swap_index <- which.max(fitness_values)
    best_swap <- swap_matrix[best_swap_index, c("card_to_remove", "card_to_add")]
    best_deck <- c(solution[solution != best_swap[[1]]], best_swap[[2]])
    
    solution <- best_deck
  }
  
  return(solution)
}

#setequal(local_search(1:30, 160, 2, function(deck_matrix) apply(deck_matrix, 1, function(deck) sum(158 %in% deck, 159 %in% deck))), c(3:30, 158, 159))

rs_evaluation_fn <- function(cardlist, log_file, use_last_n_logs, model_definition) {
  feature_fn_applied <- model_definition$feature_fn(cardlist)
  
  log_train <- read_log_matches(log_file, use_last_n_logs/2)
  train_data <- prepare_log(log_train, feature_fn_applied)
  m <- model_definition$model_fn(train_data)
  
  return(function(decks_matrix) predict(m, bind_rows(apply(decks_matrix, 1, feature_fn_applied)), type = "response"))
}

#rs_evaluation_fn(read_cardlist(), "log_scenario1.njson", 1000, 0.8, aggregates_model)(matrix(c(1:30, 61:90), nrow=2))

build_rs <- function(cardlist, log_file, history_length, model_definition) {
  if (is.null(model_definition)) {
    evaluation_fn <- NULL
  } else {
    evaluation_fn <- tryCatch({
      rs_evaluation_fn(cardlist, log_file, history_length, model_definition)
    }, error=function(cond) {
      print(cond)
      return(NULL)
    })
  }

  if (is.null(evaluation_fn)) {
    return(function(deck, neighborhood_size=6) deck)
  } else {
    return(function(deck, neighborhood_size=6) local_search(deck, nrow(cardlist), neighborhood_size, evaluation_fn))
  }
}

#build_rs(read_cardlist(), "simulation_debug.njson", 1000, NULL)(1:30)
#build_rs(read_cardlist(), "simulation_debug.njson", 1000, aggregates_model)(1:30)

model_fn_with_intercept <- function(train_data) glm(train_data$target ~ ., train_data$features, family = "binomial")
model_fn_without_intercept <- function(train_data) glm(train_data$target ~ . -1, train_data$features, family = "binomial")

# =============================================================================
#  Regression model with aggregate card statistics
# =============================================================================

transform_to_aggregates <- function(cardlist) {
  is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
  
  return(function (cards, enemy_cards) {
    if (length(cards) != NUM_HAND_CARDS) {
      stop(paste("Invalid number of cards, expected", NUM_HAND_CARDS, "but got", length(cards)))
    }
    
    selected_cards <- cardlist[cards, ]
    creature <- selected_cards[selected_cards$type == "creature", ]
    itemGreen <- selected_cards[selected_cards$type == "itemGreen", ]
    itemRed <- selected_cards[selected_cards$type == "itemRed", ]
    itemBlue <- selected_cards[selected_cards$type == "itemBlue", ]
    
    features <- data.frame(
      #type_creature_count = sum(cardlist[cards, "type"] == "creature"),
      type_itemGreen_count = nrow(itemGreen),
      type_itemBlue_count = nrow(itemBlue),
      type_itemRed_count = nrow(itemRed),
      creature_attack_mean = mean(creature$attack),
      itemGreen_attack_mean = mean(itemGreen$attack),
      itemRed_attack_mean = mean(itemRed$attack),
      #itemBlue_attack_mean = mean(itemBlue$attack), # gibt's nicht
      creature_defense_mean = mean(creature$defense),
      itemGreen_defense_mean = mean(itemGreen$defense),
      itemBlue_defense_mean = mean(itemBlue$defense),
      itemRed_defense_mean = mean(itemRed$defense),
      ability_b_count = sum(itemGreen$ability_b) + sum(creature$ability_b),
      ability_c_count = sum(itemGreen$ability_c) + sum(creature$ability_c),
      ability_d_count = sum(itemGreen$ability_d) + sum(creature$ability_d),
      ability_g_count = sum(itemGreen$ability_g) + sum(creature$ability_g),
      ability_l_count = sum(itemGreen$ability_l) + sum(creature$ability_l),
      ability_w_count = sum(itemGreen$ability_w) + sum(creature$ability_w),
      player_hp_mean = mean(cardlist[cards, "playerHP"]),
      enemy_hp_mean = mean(cardlist[cards, "enemyHP"]),
      card_draw_mean = mean(cardlist[cards, "cardDraw"]),
      # Manakurve
      cost_mean = mean(cardlist[cards, "cost"]),
      cost_sd = sd(cardlist[cards, "cost"])
    )
    
    features[is.nan(features)] <- 0
    
    return(features)
  })
}

aggregates_model <- list(
  name="Aggregates",
  feature_fn=transform_to_aggregates,
  model_fn=model_fn_with_intercept
)


# =============================================================================
#  Regression model with utility values
# =============================================================================

transform_to_utilities <- function(cardlist) {
  calculate_card_synergy_score <- function(card1Id, card2Id) {
    card1 <- cardlist[card1Id, ]
    card2 <- cardlist[card2Id, ]
    
    # +1 for each synergy
    score <- c(
      # low attack gains lethal for instant kill
      card1$type == "creature" & card1$attack <= 2 & !card1$ability_l & card2$type == "itemGreen" & card2$ability_l,
      # high attack gains breakthrough for enemey player damage
      card1$type == "creature" & card1$attack >= 4 & !card1$ability_b & card2$type == "itemGreen" & card2$ability_b,
      # high attack gains drain to heal the player
      card1$type == "creature" & card1$attack >= 4 & !card1$ability_d & card2$type == "itemGreen" & card2$ability_d,
      # high attack gains charge
      card1$type == "creature" & card1$attack >= 4 & !card1$ability_c & card2$type == "itemGreen" & card2$ability_c,
      # low defense gains ward for invulnerability
      card1$type == "creature" & card1$defense <= 2 & !card1$ability_w & card2$type == "itemGreen" & card2$ability_w,
      # high defense gains guard to become primary target
      card1$type == "creature" & card1$defense >= 4 & !card1$ability_g & card2$type == "itemGreen" & card2$ability_g,
      # apply red item to enemy, then charge
      card1$type == "creature" & card1$ability_c & card2$type == "redItem",
      # remove guard with lethal, attack with drain
      card1$type == "creature" & card1$ability_l & card2$type == "creature" & card2$ability_d,
      # guard gets ward and vice versa
      card1$type == "creature" & card1$ability_g & !card1$ability_w & card2$type == "itemGreen" & card2$ability_w,
      card1$type == "creature" & card1$ability_w & !card1$ability_g & card2$type == "itemGreen" & card2$ability_g
    )
    return(score)
  }
  
  calculate_deck_synergy_score <- function(cards) {
    tuples <- combn(cards, 2)
    tuple_scores <- calculate_card_synergy_score(tuples[1, ], tuples[2, ]) + calculate_card_synergy_score(tuples[2, ], tuples[1, ])
    return(mean(tuple_scores)*100/2)
  }
  
  #scores <- sapply(1:100, function(x) calculate_deck_synergy_score(sample(1:151, 30)))
  #hist(scores)
  # observation: mean score is ~30, some decks are 60+
  
  # "lm", "svmLinear2", "rpart1SE"
  card_cost_model <- train(cost ~ . - name - description, cardlist, method="rpart1SE", model=TRUE)
  #rpart.plot(card_cost_model$finalModel)
  # observation: cost effectivity solely depends on attack and defense
  card_cost_effectivity <- (predict(card_cost_model, cardlist) - cardlist$cost) / 4
  #hist(card_cost_effectivity)
  # observation: mean cost effectivity is 1.0, though some cards are close to 0.0 or 2.0
  
  return(function(cards, enemy_cards) {
    if (length(cards) != NUM_HAND_CARDS) {
      stop(paste("Invalid number of cards, expected", NUM_HAND_CARDS, "but got", length(cards)))
    }
    
    cost_effectiveness <- sum(card_cost_effectivity[cards])
    synergy_score <- calculate_deck_synergy_score(cards)
    
    creature_count <- sum(cardlist[cards, "type"] == "creature")
    itemGreen_count <- sum(cardlist[cards, "type"] == "itemGreen")
    # Coac: min. 40% creatures
    strategy_parameter <- abs(creature_count / NUM_HAND_CARDS - 0.7) / 0.3 + abs(itemGreen_count / NUM_HAND_CARDS - 0.3) / 0.7
    
    cost <- cardlist[cards, "cost"]
    mana <- abs(mean(cost) - 3.8) # Coac: max. 6 mana average
    
    return(data.frame(
      cost_effectiveness = cost_effectiveness,
      synergy_score = synergy_score,
      strategy_parameter = strategy_parameter,
      mana = mana
    ))
  })
}

utilities_model <- list(
  name="Utility",
  feature_fn=transform_to_utilities,
  model_fn=model_fn_with_intercept
)

utilities_model$model_fn(prepare_log(read_log_matches("logs/train_log.njson", 5000), utilities_model$feature_fn(read_cardlist())))

# =============================================================================
#  Bag of Cards
# =============================================================================

transform_to_bagofcards <- function(cardlist) {
  return(function(cards, enemy_cards) {
    if (length(cards) != NUM_HAND_CARDS) {
      stop(paste("Invalid number of cards, expected", NUM_HAND_CARDS, "but got", length(cards)))
    }
    
    num_cards <- nrow(cardlist)
    genotype <- integer(num_cards)
    genotype[cards] <- 1
    df <- data.frame(t(genotype))
    colnames(df) <- paste("card", 1:num_cards, sep="_")
    return(df)
  })
}

train_bagofcards <- function(train_data) {
  features <- train_data$features
  features[is.na(features)] <- 0
  #selected_features <- train_data$features[, colSums(train_data$features) > 50]
  m <- glm(train_data$target ~ . -1, features, family = "binomial")
  return(m)
}

bagofcards_model <- list(
  name="Bag of Cards",
  feature_fn=transform_to_bagofcards,
  model_fn=train_bagofcards
)


# =============================================================================
#  Ranking list
# =============================================================================

transform_to_ranking <- function(cardlist) {
  return(function(cards, enemy_cards) {
    if (length(cards) != NUM_HAND_CARDS) {
      stop(paste("Invalid number of cards, expected", NUM_HAND_CARDS, "but got", length(cards)))
    }
    
    ranks <- which(tier_list %in% cards)
    mean_rank <- mean(ranks)
    return(data.frame(mean_rank=mean_rank))
  })
}

#transform_to_ranking(read_cardlist())(1:30)
#transform_to_aggregates(read_cardlist())(1:30)
#ranking_model$model_fn(prepare_log(read_log_matches("log_scenario1.njson", 100), ranking_model$feature_fn(read_cardlist())))

ranking_model <- list(
  name="Ranking",
  feature_fn=transform_to_ranking,
  model_fn=model_fn_with_intercept
)

