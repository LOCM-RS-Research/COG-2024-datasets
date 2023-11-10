###
# script to evaluate different variants of the utility model
#

source("setup.R")

calculate_card_synergy_score <- function(cardlist, card1Id, card2Id) {
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

calculate_deck_synergy_score <- function(cardlist, cards) {
  tuples <- combn(cards, 2)
  tuple_scores <- calculate_card_synergy_score(cardlist, tuples[1, ], tuples[2, ]) + calculate_card_synergy_score(cardlist, tuples[2, ], tuples[1, ])
  return(mean(tuple_scores)*100/2)
}

###
#
#

transform_to_utilities_1 <- function(cardlist) {
  card_cost_model <- train(cost ~ . - name - description, cardlist, method="rpart1SE", model=TRUE)
  card_cost_effectivity <- (predict(card_cost_model, cardlist) - cardlist$cost) / 4

  return(function(cards, enemy_cards) {
    if (length(cards) != NUM_HAND_CARDS) {
      stop(paste("Invalid number of cards, expected", NUM_HAND_CARDS, "but got", length(cards)))
    }
    
    cost_effectiveness <- sum(card_cost_effectivity[cards])
    synergy_score <- calculate_deck_synergy_score(cardlist, cards)
    
    creature_count <- sum(cardlist[cards, "type"] == "creature")
    itemGreen_count <- sum(cardlist[cards, "type"] == "itemGreen")
    strategy_parameter <- abs(creature_count / NUM_HAND_CARDS - 0.7) / 0.3 + abs(itemGreen_count / NUM_HAND_CARDS - 0.3) / 0.7
    
    cost <- cardlist[cards, "cost"]
    mana <- abs(mean(cost) - 3.8) # Coac: max. 6 mana on average
    
    return(data.frame(
      cost_effectiveness = cost_effectiveness,
      synergy_score = synergy_score,
      strategy_parameter = strategy_parameter,
      mana = mana
    ))
  })
}

m <- model_fn_with_intercept(prepare_log(read_log_matches("logs/train_log.njson", 10000), transform_to_utilities_1(read_cardlist())))
m
summary(m)


transform_to_utilities_2 <- function(cardlist) {
  card_cost_model <- train(cost ~ . - name - description, cardlist, method="rpart1SE", model=TRUE)
  card_cost_effectivity <- (predict(card_cost_model, cardlist) - cardlist$cost) / 4
  
  return(function(cards, enemy_cards) {
    if (length(cards) != NUM_HAND_CARDS) {
      stop(paste("Invalid number of cards, expected", NUM_HAND_CARDS, "but got", length(cards)))
    }
    
    cost_effectiveness <- sum(card_cost_effectivity[cards])
    synergy_score <- calculate_deck_synergy_score(cardlist, cards)
    
    creature_count <- sum(cardlist[cards, "type"] == "creature")
    strategy_parameter <- creature_count / NUM_HAND_CARDS
    
    cost <- cardlist[cards, "cost"]
    mana <- mean(cost)
    
    return(data.frame(
      cost_effectiveness = cost_effectiveness,
      synergy_score = synergy_score,
      strategy_parameter = strategy_parameter,
      mana = mana
    ))
  })
}

model_fn_with_intercept(prepare_log(read_log_matches("logs/train_log.njson", 10000), transform_to_utilities_2(read_cardlist())))


transform_to_utilities_3 <- function(cardlist) {
  card_cost_model <- train(cost ~ . - name - description, cardlist, method="rpart1SE", model=TRUE)
  card_cost_effectivity <- (predict(card_cost_model, cardlist) - cardlist$cost) / 4
  
  return(function(cards, enemy_cards) {
    if (length(cards) != NUM_HAND_CARDS) {
      stop(paste("Invalid number of cards, expected", NUM_HAND_CARDS, "but got", length(cards)))
    }
    
    cost_effectiveness <- sum(card_cost_effectivity[cards])
    synergy_score <- calculate_deck_synergy_score(cardlist, cards)
    
    creature_count <- sum(cardlist[cards, "type"] == "creature")
    strategy_parameter <- abs(creature_count / NUM_HAND_CARDS - 0.4)
    
    cost <- cardlist[cards, "cost"]
    mana <- abs(mean(cost) - 3.8) # Coac: max. 6 mana on average
    
    return(data.frame(
      cost_effectiveness = cost_effectiveness,
      synergy_score = synergy_score,
      strategy_parameter = strategy_parameter,
      mana = mana
    ))
  })
}

model_fn_with_intercept(prepare_log(read_log_matches("logs/train_log.njson", 10000), transform_to_utilities_3(read_cardlist())))


transform_to_utilities_4 <- function(cardlist) {
  card_cost_model <- train(cost ~ . - name - description, cardlist, method="rpart1SE", model=TRUE)
  card_cost_effectivity <- (predict(card_cost_model, cardlist) - cardlist$cost) / 4
  
  return(function(cards, enemy_cards) {
    if (length(cards) != NUM_HAND_CARDS) {
      stop(paste("Invalid number of cards, expected", NUM_HAND_CARDS, "but got", length(cards)))
    }
    
    cost_effectiveness <- sum(card_cost_effectivity[cards])
    synergy_score <- calculate_deck_synergy_score(cardlist, cards)
    
    creature_count <- sum(cardlist[cards, "type"] == "creature")
    strategy_parameter <- 1 - abs(creature_count / NUM_HAND_CARDS - 0.7)
    
    cost <- cardlist[cards, "cost"]
    mana <- 1 - abs(mean(cost) - 3.8) # Coac: max. 6 mana on average
    
    return(data.frame(
      cost_effectiveness = cost_effectiveness,
      synergy_score = synergy_score,
      strategy_parameter = strategy_parameter,
      mana = mana
    ))
  })
}

model_fn_with_intercept(prepare_log(read_log_matches("logs/train_log.njson", 10000), transform_to_utilities_4(read_cardlist())))


transform_to_utilities_5 <- function(cardlist) {
  card_cost_model <- train(cost ~ . - name - description, cardlist, method="rpart1SE", model=TRUE)
  card_cost_effectivity <- (predict(card_cost_model, cardlist) - cardlist$cost) / 4
  
  return(function(cards, enemy_cards) {
    if (length(cards) != NUM_HAND_CARDS) {
      stop(paste("Invalid number of cards, expected", NUM_HAND_CARDS, "but got", length(cards)))
    }
    
    cost_effectiveness <- sum(card_cost_effectivity[cards])
    synergy_score <- calculate_deck_synergy_score(cardlist, cards)
    
    cost <- cardlist[cards, "cost"]
    mana <- abs(mean(cost) - 3.8) # Coac: max. 6 mana on average
    
    return(data.frame(
      cost_effectiveness = cost_effectiveness,
      synergy_score = synergy_score,
      strategy_parameter = 0,
      mana = mana
    ))
  })
}

model_fn_with_intercept(prepare_log(read_log_matches("logs/train_log.njson", 10000), transform_to_utilities_5(read_cardlist())))
