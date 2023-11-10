###
# Preparation of scenario experiments
#

source("setup.R")

# Evaluation
aggregate_test_data <- function(decks_filename, log_filename, output_filename) {
  test_log <- read_log_matches(log_filename)
  test_decks <- lapply(readLines(decks_filename, warn=FALSE), function(l) as.numeric(unlist(strsplit(l, ",", fixed=TRUE))))
  
  test_decks_logs <- test_log %>%
    filter(deck %in% test_decks)
  
  test_decks_results <- test_decks_logs %>%
    group_by(deck) %>%
    summarise(
      matches=length(deck),
      wins=sum(victory),
      win_rate=mean(victory),
      sd_win_rate=sd(victory),
      mean_health_difference_norm=mean((health_difference+30)/60),
      sd_health_difference_norm=sd((health_difference+30)/60)
    ) %>%
    mutate(deckid = sapply(deck, function(x) paste(sort(x), collapse="-"))) %>%
    arrange(-win_rate) %>%
    mutate(rank_by_win_rate=1:n()) %>%
    arrange(-mean_health_difference_norm) %>%
    mutate(rank_by_mean_health_difference=1:n())
  
  write.csv2(test_decks_results, file=output_filename)
  return(test_decks_results)
}

###
# Scenario 1

cardlist_1 <- read_cardlist()

# Preparation
#random_decks_train <- lapply(1:10000, function(x) paste(sample(1:nrow(cardlist_1), NUM_HAND_CARDS), collapse=","))
#cat(paste(random_decks_train, collapse="\n"), file = "logs/train_decks.txt")
#
#random_decks_test <- lapply(1:100, function(x) paste(sample(1:nrow(cardlist_1), NUM_HAND_CARDS), collapse=","))
#cat(paste(random_decks_test, collapse="\n"), file = "logs/test_decks.txt")
#
#random_opponents <- lapply(1:10000, function(x) paste(sample(1:nrow(cardlist_1), NUM_HAND_CARDS), collapse=","))
#cat(paste(random_opponents, collapse="\n"), file = "logs/opponents.txt")

# ... use a bash script to play these games ...

test_decks_results <- aggregate_test_data("logs/test_decks.txt", "logs/test_log.njson", "logs/test_decks_results.csv") # takes 10-20 minutes at 3GB log data


###
# Scenario 2

# add strong new cards

new_cards_spec <- "
Shiny Crossbow;itemRed;3;0;-6;------;0;-2;0
Shiny Dagger;itemRed;2;-1;-1;BCDGLW;+1;-1;1
Shiny Shuriken;itemRed;4;-5;-5;BCDGLW;0;-1;1
Shiny Rock;itemRed;2;-4;-3;BCDGLW;0;0;1

Shiny Candy;itemGreen;3;1;4;---G--;0;0;1
Shiny Cupcake;itemGreen;3;6;3;-C----;0;0;0
Shiny Shield;itemGreen;3;2;6;---G--;0;0;0
Shiny Vaccine;itemGreen;4;6;4;BC----;3;0;0
Shiny Soda;itemGreen;4;3;3;-----W;2;0;1

Shiny Scroll;itemBlue;3;0;-2;------;2;-2;1
Shiny Acid;itemBlue;2;0;-2;------;0;-3;0

Shiny Flurpel;creature;4;5;5;-C----;0;0;0
Shiny Flomsel;creature;5;2;9;---G--;0;0;1
Shiny Flimpa;creature;4;7;1;-C----;1;0;0
Shiny Swonsh;creature;1;1;2;-C-G--;0;0;0
Shiny Slumsh;creature;2;3;3;B-----;0;0;1
"
new_cards <- read.csv2(text=new_cards_spec, col.names=c("name", "type", "cost", "attack", "defense", "abilities", "playerHP", "enemyHP", "cardDraw"))
new_cards$ability_b = grepl("B", new_cards$abilities, fixed=TRUE)
new_cards$ability_c = grepl("C", new_cards$abilities, fixed=TRUE)
new_cards$ability_d = grepl("D", new_cards$abilities, fixed=TRUE)
new_cards$ability_g = grepl("G", new_cards$abilities, fixed=TRUE)
new_cards$ability_l = grepl("L", new_cards$abilities, fixed=TRUE)
new_cards$ability_w = grepl("W", new_cards$abilities, fixed=TRUE)
new_cards$abilities <- NULL
new_cards$description <- ""

cardlist_2 <- rbind(cardlist_1, new_cards)
rownames(cardlist_2) <- NULL # reset rownames
write_cardlist(cardlist_2, "logs/cardlist_new_card2.txt")

# random_decks_train_2 <- lapply(1:10000, function(x) paste(sample(1:nrow(cardlist_2), NUM_HAND_CARDS), collapse=","))
# cat(paste(random_decks_train_2, collapse="\n"), file = "logs/train_decks_new_card.txt")
# 
# decks_2_test <- lapply(1:100, function(x) {
#   deck <- c()
#   while (length(intersect(deck, 161:nrow(cardlist_2))) == 0) {
#     deck <- sample(1:nrow(cardlist_2), NUM_HAND_CARDS)
#   }
#   return(paste(deck, collapse=","))
# })
# cat(paste(decks_2_test, collapse="\n"), file = "logs/test_decks_new_card.txt")


# ... execute with bash script ...

test_decks_results_newcard_biased <- aggregate_test_data("logs/test_decks_new_card.txt", "logs/test_log_new_card_biased.njson", "logs/test_decks_results_new_card_biased.csv")
test_decks_results_newcard_biased_2 <- aggregate_test_data("logs/test_decks_new_card.txt", "logs/test_log_new_card_2.njson", "logs/test_decks_results_new_card_biased_2.csv")

###
# Scenario 3

cardlist_3 <- cardlist_1

# Result of exploratory analysis of V1:
# 18, 123, 124, 69 strong cards
# 110, 140, 57, 108 weak cards
# 7, 127, 83, 21 mediocre cards
# green cards with attack, D or W are strong
# creatures with G or card draw are weak

#
# nerf strong cards
#
# aggressive creature
cardlist_3[18, ]$cost <- 5
cardlist_3[18, ]$defense <- 3
cardlist_3[18, ]$attack <- 6
# green item gets attack - rework to defensive item
cardlist_3[123, ]$cost <- 3
cardlist_3[123, ]$attack <- 1
cardlist_3[123, ]$defense <- 4
cardlist_3[123, ]$ability_g <- TRUE
# green item gets attack, defense and drain
cardlist_3[124, ]$attack <- 1
cardlist_3[124, ]$defense <- 0
# creature with B, attack and defense
cardlist_3[69, ]$defense <- 1
cardlist_3[69, ]$cost <- 4

#
# buff weak cards
#
# very defensive creature
cardlist_3[57, ]$cost <- 3
cardlist_3[57, ]$attack <- 3
cardlist_3[57, ]$ability_g <- TRUE
# green item gets charge
cardlist_3[140, ]$cost <- 1
cardlist_3[140, ]$attack <- 2
cardlist_3[140, ]$cardDraw <- 1
# defensive creature with guard
cardlist_3[110, ]$cost <- 3
cardlist_3[110, ]$attack <- 3
cardlist_3[110, ]$cardDraw <- 1
cardlist_3[110, ]$playerHP <- 1
# defensive creature with guard, rework to aggressive creature
cardlist_3[108, ]$cost <- 2
cardlist_3[108, ]$attack <- 4
cardlist_3[108, ]$defense <- 2
cardlist_3[108, ]$cardDraw <- 1
cardlist_3[108, ]$enemyHP <- -1
cardlist_3[108, ]$ability_g <- FALSE
cardlist_3[108, ]$ability_c <- TRUE

#
# buff mediocre cards
#
# creature with ward - rework
cardlist_3[7, ]$ability_g <- TRUE
cardlist_3[7, ]$ability_w <- FALSE
cardlist_3[7, ]$defense <- 4
cardlist_3[7, ]$attack <- 2
# green item with a lot of defense
cardlist_3[127, ]$cost <- 2
cardlist_3[127, ]$cardDraw <- 1
#
# nerf mediocre cards
#
# cheap creature with low attack, defense and C
cardlist_3[83, ]$ability_c <- FALSE
# expensive creature with high attack and defense
cardlist_3[21, ]$attack <- 2


#card_cost_model <- train(cost ~ . - name - description, cardlist_3, method="rpart1SE", model=TRUE)
#cardlist_3$effectivity <- (predict(card_cost_model, cardlist_3) - cardlist_3$cost) / 4

buffed_cards <- c(57, 140, 110, 108, 7, 127)
nerfed_cards <- c(18, 123, 124, 69, 83, 21)
balanced_cards <- c(buffed_cards, nerfed_cards)

# random_decks_3_test <- lapply(1:100, function(x) {
#   deck <- c()
#   while (length(intersect(deck, balanced_cards)) == 0) {
#     deck <- sample(1:nrow(cardlist_3), NUM_HAND_CARDS)
#   }
#   return(paste(deck, collapse=","))
# })
# cat(paste(random_decks_3_test, collapse="\n"), file = "logs/test_decks_balance2.txt")

write_cardlist(cardlist_3, "logs/cardlist_balance2.txt")

# ... execution ...

test_decks_results_balance_biased <- aggregate_test_data("logs/test_decks_balance.txt", "logs/test_log_balance_biased.njson", "logs/test_decks_results_balance_biased.csv")
test_decks_results_balance_random <- aggregate_test_data("logs/test_decks.txt", "logs/test_log_balance_random.njson", "logs/test_decks_results_balance_random.csv")

###
# Scenario 4

# Enemies play more G-creatures or G-items (expectation: red item with G improves, green item or creature with L gets worse)

#trend_opponents <- lapply(1:10000, function(x) paste(sample(1:nrow(cardlist_1), NUM_HAND_CARDS, prob=ifelse(cardlist_1$type %in% c("creature", "itemGreen") & cardlist_1$ability_g, 5.0, 1.0)), collapse=","))
#cat(paste(trend_opponents, collapse="\n"), file = "logs/opponents_trend.txt")

# alternative: enemies play more high attack cards
#trend_opponents <- lapply(1:10000, function(x) paste(sample(1:nrow(cardlist_1), NUM_HAND_CARDS, prob=ifelse(cardlist_1$attack > cardlist_1$defense, 5, 1)), collapse=","))
#cat(paste(trend_opponents, collapse="\n"), file = "logs/opponents_trend_2.txt")

# third attempt: enemies play creatures only
#trend_opponents <- lapply(1:10000, function(x) paste(sample(1:nrow(cardlist_1), NUM_HAND_CARDS, prob=ifelse(cardlist_1$type == "creature", 1, 0)), collapse=","))
#cat(paste(trend_opponents, collapse="\n"), file = "logs/opponents_trend_3.txt")


# decks should have more blue items because they are affected most
#decks_4_test <- lapply(1:100, function(x) paste(sample(1:nrow(cardlist_1), NUM_HAND_CARDS, prob=ifelse(cardlist_1$type == "itemBlue", 5.0, 1.0)), collapse=","))
#cat(paste(decks_4_test, collapse="\n"), file = "logs/test_decks_trend.txt")

# new test decks: contain at least 3 of 10 cards which win rates are affected the most
# most_impacted_cards_4 <- c(46, 114, 88, 44, 69, 129, 79, 116, 59, 80)
# random_decks_4_test <- lapply(1:100, function(x) {
#   deck <- c()
#   while (length(intersect(deck, most_impacted_cards_4)) <= 2) {
#     deck <- sample(1:nrow(cardlist_1), NUM_HAND_CARDS)
#   }
#   return(paste(deck, collapse=","))
# })
# cat(paste(random_decks_4_test, collapse="\n"), file = "logs/test_decks_trend_neu.txt")

# new test decks (2): contain at least 1 of 10 cards which win rates are affected the most
# most_impacted_cards_4 <- c(46, 114, 88, 44, 69, 129, 79, 116, 59, 80)
# random_decks_4_test <- lapply(1:100, function(x) {
#   deck <- c()
#   while (length(intersect(deck, most_impacted_cards_4)) == 0) {
#     deck <- sample(1:nrow(cardlist_1), NUM_HAND_CARDS)
#   }
#   return(paste(deck, collapse=","))
# })
# cat(paste(random_decks_4_test, collapse="\n"), file = "logs/test_decks_trend_neu_neu.txt")

# new test decks (2): contain at least 4 of 10 cards which win rates are affected the most
# most_impacted_cards_4 <- c(62, 81, 116, 114, 77, 82, 11, 140, 64, 132)
# random_decks_4_test <- lapply(1:100, function(x) {
#   deck <- c()
#   while (length(intersect(deck, most_impacted_cards_4)) <= 3) {
#     deck <- sample(1:nrow(cardlist_1), NUM_HAND_CARDS)
#   }
#   return(paste(deck, collapse=","))
# })
# cat(paste(random_decks_4_test, collapse="\n"), file = "logs/test_decks_trend_neu_neu_neu.txt")


# ... execution ...

test_decks_results_trend_random <- aggregate_test_data("logs/test_decks.txt", "logs/test_log_trend_2_random.njson", "logs/test_decks_results_trend_2_random.csv")
test_decks_results_trend_biased <- aggregate_test_data("logs/test_decks_trend.txt", "logs/test_log_trend_biased.njson", "logs/test_decks_results_trend_biased.csv")
test_decks_results_trend_biased_neu <- aggregate_test_data("logs/test_decks_trend_neu.txt", "logs/test_log_trend_biased_neu.njson", "logs/test_decks_results_trend_biased_neu.csv")
test_decks_results_trend_biased_neu_neu <- aggregate_test_data("logs/test_decks_trend_neu_neu.txt", "logs/test_log_trend_biased_neu_neu.njson", "logs/test_decks_results_trend_biased_neu_neu.csv")
test_decks_results_trend_biased_neu_neu_neu <- aggregate_test_data("logs/test_decks_trend_neu_neu_neu.txt", "logs/test_log_trend_biased_neu_neu_neu.njson", "logs/test_decks_results_trend_biased_neu_neu_neu.csv")


###
# Various EDA

# correlation between Win Rate and Mean Health Difference
test_decks_results %>%
  ggplot(aes(x=win_rate, y=mean_health_difference_norm)) + geom_point()

# dependency between Rank and Health Difference is about linear
test_decks_results %>%
  ggplot(aes(x=rank_by_win_rate)) + geom_point(aes(y=mean_health_difference_norm, color="red")) + geom_point(aes(y=win_rate, color="blue"))

test_decks_results %>%
  ggplot(aes(x=rank_by_mean_health_difference)) + geom_point(aes(y=mean_health_difference_norm, color="red")) + geom_point(aes(y=win_rate, color="blue"))

# ca. 280-300 matches per Test-Deck
test_decks_results %>%
  ggplot(aes(x=deckid, y=matches)) + geom_bar(stat="identity")

# win rate is approx. normally distributed
test_decks_results %>%
  ggplot(aes(x=win_rate)) + geom_histogram()

# mean health difference as well
test_decks_results %>%
  ggplot(aes(x=mean_health_difference_norm)) + geom_histogram()

# SD for health difference slightly lower
test_decks_results %>%
  select(sd_health_difference_norm, sd_win_rate) %>%
  summarise_all(list(Min = min, Mean = mean, Max = max))


###
# Simulation 2

# cards developed for scenario 2 replace some random cards of the same type

cardlist_sim2 <- cardlist_1
# 
# new_cards_raw <- new_cards
# reds_to_replace <- sample(which(cardlist_1$type == "itemRed"), sum(new_cards_raw$type == "itemRed"))
# cardlist_sim2[reds_to_replace, ] <- new_cards_raw[new_cards_raw$type == "itemRed", colnames(cardlist_sim2)]
# blues_to_replace <- sample(which(cardlist_1$type == "itemBlue"), sum(new_cards_raw$type == "itemBlue"))
# cardlist_sim2[blues_to_replace, ] <- new_cards_raw[new_cards_raw$type == "itemBlue", colnames(cardlist_sim2)]
# greens_to_replace <- sample(which(cardlist_1$type == "itemGreen"), sum(new_cards_raw$type == "itemGreen"))
# cardlist_sim2[greens_to_replace, ] <- new_cards_raw[new_cards_raw$type == "itemGreen", colnames(cardlist_sim2)]
# creatures_to_replace <- sample(which(cardlist_1$type == "creature"), sum(new_cards_raw$type == "creature"))
# cardlist_sim2[creatures_to_replace, ] <- new_cards_raw[new_cards_raw$type == "creature", colnames(cardlist_sim2)]
# 
# write_cardlist(cardlist_sim2, "logs/cardlist_simulation_2.txt")
