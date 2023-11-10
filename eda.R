###
# EDA of training data to build a utility model
#

#matches <- read_log_matches("logs/train_log_new_card.njson")
#CARDS <- read_cardlist("logs/cardlist_new_card.txt")
#matches <- read_log_matches("logs/train_log_balance2.njson")
#CARDS <- read_cardlist("logs/cardlist_balance2.txt")
matches <- read_log_matches_("logs/train_log.njson")
#matches <- read_log_matches_filtered("logs/train_decks.txt", "logs/train_log_trend.njson")
CARDS <- read_cardlist()

# TODO seems wrong, fix this
stats_by_card <- matches %>%
  unnest(cols=deck) %>%
  inner_join(CARDS %>% mutate(id = as.numeric(rownames(CARDS))), by = c("deck" = "id")) %>%
  mutate(
    card=deck,
    type_creature=(type=="creature"),
    type_itemGreen=(type == "itemGreen"),
    type_itemRed=(type == "itemRed"),
    type_itemBlue=(type == "itemBlue")
  ) %>%
  pivot_wider(names_from = type, values_from = c(attack, defense, ability_b, ability_c, ability_d, ability_g, ability_l, ability_w),
              values_fill = 0,
              values_fn = list(attack = sum, defense = sum, ability_b = any, ability_c = any, ability_d = any, ability_g = any, ability_l = any, ability_w = any)) %>%
  select(-c(deck, time, name, description))

# what are the best cards?
matches %>%
  unnest(cols=deck) %>%
  group_by(card=deck) %>%
  summarise(win_rate=mean(victory)) %>%
  mutate(type=CARDS[card, "type"]) %>%
  arrange(-win_rate) %>%
  print(n=160)

# how do card statistics correlate with the game outcome?
as.data.frame(cor(stats_by_card %>% select(-c(card, victory, health_difference)), data.frame(victory=stats_by_card$victory))) %>%
  arrange(-abs(victory))

# preparation for utilities model
matches %>%
  mutate(cost=sapply(deck, function(d) mean(CARDS[d, "cost"]))) %>%
  group_by(cost) %>%
  summarise(win_rate=mean(victory)) %>%
  ggplot(aes(x=cost, y=win_rate)) + geom_col()
# optimal cost: 3.8

matches %>%
  mutate(cost=sapply(deck, function(d) sd(CARDS[d, "cost"]))) %>%
  group_by(cost=floor(cost*10)/10) %>%
  summarise(win_rate=mean(victory)) %>%
  ggplot(aes(x=cost, y=win_rate)) + geom_col()
# SD: does not matter

matches %>%
  mutate(count=sapply(deck, function(d) mean(CARDS[d, "type"] == "creature"))) %>%
  group_by(count) %>%
  summarise(win_rate=mean(victory)) %>%
  ggplot(aes(x=count, y=win_rate)) + geom_col()
# optimal number of creatures: 50-90%

matches %>%
  mutate(count=sapply(deck, function(d) mean(CARDS[d, "type"] == "itemGreen"))) %>%
  group_by(count) %>%
  summarise(win_rate=mean(victory)) %>%
  ggplot(aes(x=count, y=win_rate)) + geom_col()
# optimale number of green items: 30%

matches %>%
  mutate(count=sapply(deck, function(d) mean(CARDS[d, "type"] == "itemRed"))) %>%
  group_by(count) %>%
  summarise(win_rate=mean(victory)) %>%
  ggplot(aes(x=count, y=win_rate)) + geom_col()
# optimal number of red items: 0-15% (doesn't matter)

matches %>%
  mutate(count=sapply(deck, function(d) mean(CARDS[d, "type"] == "itemBlue"))) %>%
  group_by(count) %>%
  summarise(win_rate=mean(victory)) %>%
  ggplot(aes(x=count, y=win_rate)) + geom_col()
# optimal number of blue items: 0-12% (doesn't matter)