###
# Evaluation of simulation results
#

source("setup.R")

library(dplyr)
library(ggplot2)

read_fitness_log <- function(simulation_id) {
  filename <- paste("logs/simulation_", simulation_id, ".csv", sep="")
  log <- read.csv2(filename) %>%
    mutate(
      simulation_id=simulation_id,
      rs_used=factor(rs_used, levels=0:4, labels=c("None", "Aggregates", "Utilities", "Bag of Cards", "Ranking")),
      deck=lapply(cards, function(d) eval(parse(text=d)))
    )
  return(log)
}

read_changes_log <- function(simulation_id) {
  filename <- paste("logs/simulation_", simulation_id, "_changes.csv", sep="")
  log <- read.table(
    filename,
    sep=";",
    col.names=c("unused", "iteration", "previous_cards", "cards", "rs_used", "type", "change_count")
  ) %>%
    mutate(
      simulation_id=simulation_id,
      rs_used=factor(rs_used, levels=0:4, labels=c("None", "Aggregates", "Utilities", "Bag of Cards", "Ranking")),
      type=factor(type, labels=c("crossover", "mutation", "RS-mutation")),
      deck=lapply(cards, function(d) eval(parse(text=d))),
      previous_deck=lapply(previous_cards, function(d) eval(parse(text=d)))
    )
  return(log)
}

join_card_stats <- function(log) {
  cardlist <- read_cardlist()
  
  stats_by_card <- log %>%
    unnest(cols=deck) %>%
    inner_join(cardlist %>% mutate(id = as.numeric(rownames(cardlist))), by = c("deck" = "id")) %>%
    mutate(card=deck)
  return(stats_by_card)
}

rs_colors <- scale_color_manual(values=c("None"="#F0E442", "Aggregates"="#F8766D", "Utilities"="#00BFC4", "Bag of Cards"="#7CAE00", "Ranking"="#C77CFF"))

simulation_names <- c("2023-05-20" = "FIRST", "2023-08-15" = "NEW", "2023-08-30" = "BALANCE", "2023-05-24"="NOISY", "agents_2024-10-07"="AGENTS-BALANCE", "agents_2024-10-08"="AGENTS", "agents_2024-10-09"="AGENT-BASELINE2", "agents_2024-10-24"="AGENT-BASELINE1", "agents_2024-10-25"="AGENT-R1", "agents_2024-10-26"="AGENT-RANDOM", "agents_2024-10-27"="AGENT-R2")

###
# Evaluation of simulation
#

simulation_ids_no_change <- c("2023-05-20", "2023-05-21", "2023-05-24", "2023-05-26", "2023-05-29")

simulation_id <- simulation_ids_no_change[1]

fitness_df <- read_fitness_log(simulation_id)

#
# EDA
#

# Trends in the simulation?
stats <- join_card_stats(fitness_df)

type_levels <- c("creature", "itemGreen", "itemRed", "itemBlue")
type_colors <- scale_color_manual(values=c("creature"="black", "itemGreen"="green", "itemRed"="red", "itemBlue"="blue"))

# Mean number of cards by type
stats %>%
  mutate(type_f=factor(type, levels=type_levels)) %>%
  group_by(iteration, type_f) %>%
  summarise(count=length(cards)/200, .groups="keep") %>%
  ggplot(aes(x=iteration, y=count, color=type_f)) + geom_point() + facet_grid(type_f ~ ., scales="free_y") +
  theme(legend.position="none") +
  labs(x="iteration", y="average count") + type_colors

ggsave(paste("./plots/simulation_", simulation_id, "_card_types.eps", sep=""), device="eps", width=12, height=9, units="cm", dpi="print")

# mean stats
stats %>%
  group_by(iteration) %>%
  summarise(across(
    c("attack", "defense"), function(stat) mean(stat)
  )) %>%
  pivot_longer(!iteration, names_to="statistic", values_to="mean") %>%
  mutate(statistic_f=factor(statistic, levels=c("attack", "defense"))) %>%
  ggplot(aes(x=iteration, y=mean, color=statistic_f)) + geom_point() + facet_grid(statistic_f ~ ., scales="free_y") +
  labs(x="Iteration", y="Mean") +
  theme(legend.position="none") +
  scale_color_manual(values=c("attack"="red", "defense"="blue"))

ggsave(paste("./plots/simulation_", simulation_id, "_card_statistics.eps", sep=""), device="eps", width=12, height=6, units="cm", dpi="print")

# How fast does the population converge?
jaccard_similarity <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

fitness_df %>%
  left_join(fitness_df, by=c("iteration"), suffix=c(".a", ".b"), relationship="many-to-many") %>%
  mutate(jaccard_sim=mapply(jaccard_similarity, deck.a, deck.b)) %>%
  group_by(iteration) %>%
  summarise(mean_sim=mean(jaccard_sim)) %>%
  ggplot(aes(x=iteration, y=mean_sim)) + geom_point() +
  labs(x="iteration", y="Jaccard similarity")

ggsave(paste("./plots/simulation_", simulation_id, "_convergence.eps", sep=""), device="eps", width=6, height=8, units="cm", dpi="print")

for (simulation_id in simulation_ids_no_change) {
  
fitness_df <- read_fitness_log(simulation_id)
# Number of individuals per RS
fitness_df %>%
  filter(iteration <= 25) %>%
  group_by(iteration, rs_used) %>%
  summarise(individuals=length(rs_used), .groups="keep") %>%
  ggplot(aes(x=iteration, y=individuals, color=rs_used)) + geom_bar(stat="identity") +
  labs(x="iteration", y="individuals", color="RS", caption=recode(simulation_id, !!!simulation_names)) +
  facet_grid(rs_used ~ .) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
  rs_colors

ggsave(paste("./plots/simulation_", simulation_id, "_users.eps", sep=""), device="eps", width=8, height=12, units="cm", dpi="print")

fitness_df %>%
  filter(iteration <= 25) %>%
  group_by(iteration, rs_used) %>%
  summarise(fitness=mean(fitness), .groups="keep") %>%
  ggplot(aes(x=iteration, y=fitness, color=rs_used)) + geom_point() +
  labs(x="iteration", y="average fitness", color="RS") +
  facet_grid(rs_used ~ .) +
  geom_hline(yintercept=0.5) +
  theme(legend.position="none") +
  rs_colors

ggsave(paste("./plots/simulation_", simulation_id, "_fitness.eps", sep=""), device="eps", width=12, height=12, units="cm", dpi="print")

}

###
# Evaluation of simulations with scenarios
#

simulation_ids_change <- c("2023-05-10", "2023-05-12", "2023-05-15", "2023-05-17", "2023-05-25", "2023-08-12", "2023-08-15", "2023-08-30")

for (simulation_id in simulation_ids_change) {

if (simulation_id %in% c("2023-05-10", "2023-05-12")) {
  change_iter <- 21
}
if (simulation_id %in% c("2023-05-15", "2023-05-17", "2023-08-12", "2023-08-15", "2023-08-30")) {
  change_iter <- 11
}
if (simulation_id %in% c("2023-05-25")) {
  change_iter <- 6
}
  
fitness_df <- read_fitness_log(simulation_id)
changes_df <- read_changes_log(simulation_id)

# Number of individuals per RS
fitness_df %>%
  filter(iteration <= 25) %>%
  group_by(iteration, rs_used) %>%
  summarise(individuals=length(rs_used), .groups="keep") %>%
  ggplot(aes(x=iteration, y=individuals, color=rs_used)) + geom_bar(stat="identity") +
  labs(x="iteration", y="individuals", color="RS", caption=recode(simulation_id, !!!simulation_names)) +
  geom_vline(xintercept=change_iter+0.5) +
  facet_grid(rs_used ~ .) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
  rs_colors

ggsave(paste("./plots/simulation_", simulation_id, "_users.eps", sep=""), device="eps", width=8, height=12, units="cm", dpi="print")

if (simulation_id %in% c("2023-05-10", "2023-05-12", "2023-05-15", "2023-08-30")) {
  buffed_cards <- c(57, 140, 110, 108, 7, 127)
  nerfed_cards <- c(18, 123, 124, 69, 83, 21)
}

if (simulation_id %in% c("2023-05-17", "2023-05-25", "2023-08-12", "2023-08-15")) {
  buffed_cards <- c(20, 43, 67, 69, 84, 123, 125, 128, 131, 136, 141, 149, 151, 155, 159)
  nerfed_cards <- c()
}

changes_df %>%
  filter(type=="RS-mutation") %>%
  filter(iteration <= 25) %>%
  mutate(
    cards_added=mapply(function(new, old) setdiff(new, old), deck, previous_deck),
    changed_cards_added=sapply(cards_added, function(c) sum(c %in% buffed_cards))
  ) %>%
  group_by(iteration, rs_used) %>%
  summarise(changed_cards_added=mean(changed_cards_added), .groups="keep") %>%
  ggplot(aes(x=iteration, y=changed_cards_added, color=rs_used)) + geom_point() +
  labs(x="iteration", y="avg. no. of buffed cards recommended", color="RS", caption=recode(simulation_id, !!!simulation_names)) +
  geom_vline(xintercept=change_iter-0.5) +
  facet_grid(rs_used ~ .) +
  theme(legend.position="none", plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
  rs_colors

ggsave(paste("./plots/simulation_", simulation_id, "_changed_cards.eps", sep=""), device="eps", width=12, height=10, units="cm", dpi="print")

}


# Number of individuals per RS in affected iterations
fitness_df %>%
  filter(iteration > change_iter - 5 & iteration < change_iter + 5) %>%
  group_by(iteration, rs_used) %>%
  summarise(individuals=length(rs_used), .groups="keep") %>%
  ggplot(aes(x=iteration, y=individuals, color=rs_used)) +
  geom_bar(stat="identity") +
  labs(x="iteration", y="individuals", color="RS") +
  geom_vline(xintercept=change_iter+0.5) +
  facet_grid(rs_used ~ .) +
  theme(legend.position="none") +
  rs_colors



###
# EDA
#

# Why is 2023-05-24 different?
fitness_df <- read_fitness_log("2023-05-24")
fitness_df %>%
  group_by(iteration) %>%
  left_join(fitness_df, by=c("iteration"), suffix=c(".a", ".b"), relationship="many-to-many") %>%
  mutate(jaccard_sim=mapply(jaccard_similarity, deck.a, deck.b)) %>%
  summarise(mean_sim=mean(jaccard_sim)) %>%
  ggplot(aes(x=iteration, y=mean_sim)) + geom_point() +
  labs(x="iteration", y="average Jaccard-similarity")
# after iteration 10, stronger convergence (0.3461) than in other runs (~0.15)

# Hypothesis: In the first iterations, Ranking List recommends its best cards,
# after that, it is no longer useful
rbind(read_changes_log("2023-08-15"), read_changes_log("2023-08-30"), read_changes_log("2023-05-24")) %>%
  filter(type=="RS-mutation" & iteration <= 10 & (rs_used == "Ranking" | rs_used == "Bag of Cards")) %>%
  mutate(
    simulation_id=factor(simulation_id, levels=c("2023-08-15", "2023-08-30", "2023-05-24")),
    simulation_name=recode(simulation_id, !!!simulation_names),
    cards_added=mapply(function(new, old) setdiff(new, old), deck, previous_deck),
    top5_added=sapply(cards_added, function(c) sum(tier_list[1:5] %in% c))
  ) %>%
  group_by(simulation_name, iteration, group=rs_used) %>%
  summarise(mean_top5=mean(top5_added), .groups="keep") %>%
  ggplot(aes(x=iteration, y=mean_top5, color=group)) + geom_point() +
  facet_grid(simulation_name ~ .) +
  labs(x="iteration", y="avg. no. of Ranking top 5 cards recommended", color="RS") +
  scale_x_continuous(breaks=c(0, 5, 10)) +
  theme(legend.position="bottom", legend.margin=margin())
ggsave(paste("./plots/simulation_top5s.eps", sep=""), device="eps", width=6, height=10, units="cm", dpi="print")

read_changes_log_with_agent <- function(simulation_id) {
  filename <- paste("logs/simulation_", simulation_id, "_changes.csv", sep="")
  log <- read.table(
    filename,
    sep=";",
    col.names=c("unused", "iteration", "previous_cards", "cards", "rs_used", "agent_used", "type", "change_count")
  ) %>%
    mutate(
      simulation_id=simulation_id,
      rs_used=factor(rs_used, levels=0:4, labels=c("None", "Aggregates", "Utilities", "Bag of Cards", "Ranking")),
      agent_used=factor(agent_used),
      type=factor(type, labels=c("crossover", "mutation", "RS-mutation")),
      deck=lapply(cards, function(d) eval(parse(text=d))),
      previous_deck=lapply(previous_cards, function(d) eval(parse(text=d)))
    )
  return(log)
}


buffed_cards <- c(57, 140, 110, 108, 7, 127)
nerfed_cards <- c(18, 123, 124, 69, 83, 21)

changes_df %>%
  filter(type=="RS-mutation") %>%
  filter(iteration <= 25) %>%
  mutate(
    cards_added=mapply(function(new, old) setdiff(new, old), deck, previous_deck),
    changed_cards_added=sapply(cards_added, function(c) sum(c %in% buffed_cards))
  ) %>%
  group_by(iteration, rs_used) %>%
  summarise(changed_cards_added=mean(changed_cards_added), .groups="keep") %>%
  ggplot(aes(x=iteration, y=changed_cards_added, color=rs_used)) + geom_point() +
  labs(x="iteration", y="avg. no. of buffed cards recommended", color="RS", caption=recode(simulation_id, !!!simulation_names)) +
  geom_vline(xintercept=change_iter-0.5) +
  facet_grid(rs_used ~ .) +
  theme(legend.position="none", plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
  rs_colors

ggsave(paste("./plots/simulation_", simulation_id, "_changed_cards.eps", sep=""), device="eps", width=12, height=10, units="cm", dpi="print")


###
# plots for ToG


# popularity of b/g/w ability over time in run FIRST
simulation_id <- "2023-05-20"
fitness_df <- read_fitness_log(simulation_id)
stats <- join_card_stats(fitness_df)
stats %>%
  group_by(iteration) %>%
  summarise(across(
    c("ability_g", "ability_w", "ability_b"), function(stat) sum(stat)
  )) %>%
  pivot_longer(!iteration, names_to="statistic", values_to="sum") %>%
  mutate(statistic_f=factor(statistic, levels=c("ability_b", "ability_g", "ability_w"), labels=c("Breakthrough", "Guard", "Ward"))) %>%
  ggplot(aes(x=iteration, y=sum, color=statistic_f)) + geom_point() + facet_grid(statistic %in% c("ability_g", "ability_w") ~ ., scales = "free_y") +
  labs(x="iteration", y="cards", color="ability") +
  theme(legend.position="bottom", legend.margin=margin(t = -5), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust=0.5, size=rel(1.2)), strip.text=element_blank())

ggsave(paste("../../tog-24/plots/simulation_", simulation_id, "_abilities.eps", sep=""), device="eps", width=12, height=8, units="cm", dpi="print")

# jaccard similarity in run FIRST
fitness_df %>%
  left_join(fitness_df, by=c("iteration"), suffix=c(".a", ".b"), relationship="many-to-many") %>%
  mutate(jaccard_sim=mapply(jaccard_similarity, deck.a, deck.b)) %>%
  group_by(iteration) %>%
  summarise(mean_sim=mean(jaccard_sim)) %>%
  ggplot(aes(x=iteration, y=mean_sim)) + geom_point() +
  labs(x="iteration", y="Jaccard similarity")

ggsave(paste("../../tog-24/plots/simulation_", simulation_id, "_convergence.eps", sep=""), device="eps", width=6, height=8, units="cm", dpi="print")

# individuals by RS and agent in run AGENTS
simulation_id <- "agents_2024-10-08"
fitness_df <- read_fitness_log(simulation_id)
fitness_df %>%
  filter(iteration <= 20) %>%
  group_by(iteration, rs_used, agent_used) %>%
  summarise(individuals=n(), .groups="keep") %>%
  ggplot(aes(x=iteration, y=individuals, fill=agent_used)) + geom_bar(stat="identity", position="stack") +
  labs(x="iteration", y="individuals", fill="agent", color="RS", caption=recode(simulation_id, !!!simulation_names)) +
  facet_grid(rs_used ~ .) +
  theme(legend.position="right", legend.margin=margin(t = -5), plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust=0.5, size=rel(1.2)))

ggsave(paste("../../tog-24/plots/simulation_", simulation_id, "_individuals.eps", sep=""), device="eps", width=11, height=12, units="cm", dpi="print")

# individuals by RS in run AGENT-BL2
simulation_id <- "agents_2024-10-09"
fitness_df <- read_fitness_log(simulation_id)
fitness_df %>%
  filter(iteration <= 50) %>%
  group_by(iteration, rs_used) %>%
  summarise(individuals=n(), .groups="keep") %>%
  ggplot(aes(x=iteration, y=individuals, color=rs_used)) + geom_bar(stat="identity") +
  labs(x="iteration", y="individuals", fill="agent", color="RS", caption=recode(simulation_id, !!!simulation_names)) +
  facet_grid(rs_used ~ .) +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust=0.5, size=rel(1.2))) +
  rs_colors

ggsave(paste("../../tog-24/plots/simulation_", simulation_id, "_users.eps", sep=""), device="eps", width=8, height=12, units="cm", dpi="print")

# table with mean/sd of aggregate RS model's coefficients in the first 10 iterations for each run
coeff_means_iter <- 10
coeff_means <- lapply(c("simulation_agents_2024-10-09.njson", "simulation_agents_2024-10-24.njson", "simulation_agents_2024-10-25.njson", "simulation_agents_2024-10-26.njson", "simulation_agents_2024-10-27.njson"), function(logfile) {
  model_definition <- aggregates_model
  feature_fn_applied <- model_definition$feature_fn(read_cardlist("cardlist.txt"))
  train_log <- read_log_matches(logfile)
  train_log_piece <- train_log[1:(coeff_means_iter*4000), ]
  train_data <- prepare_log(train_log_piece, feature_fn_applied)
  
  models_by_iteration <- lapply(1:coeff_means_iter, function(i) {
    end_index <- i * length(train_data$target) / coeff_means_iter
    start_index <- max(1, end_index - 4000)
    train_chunk <- list(target=train_data$target[start_index:end_index], features=train_data$features[start_index:end_index,])
    m <- model_definition$model_fn(train_chunk)
    return (m)
  })
  
  attack_values <- sapply(models_by_iteration, function(m) m$coefficients["creature_attack_mean"])
  attack_mean <- mean(attack_values)
  attack_sd <- sd(attack_values)
  
  defense_values <- sapply(models_by_iteration, function(m) m$coefficients["creature_defense_mean"])
  defense_mean <- mean(defense_values)
  defense_sd <- sd(defense_values)
  
  ability_g_values <- sapply(models_by_iteration, function(m) m$coefficients["ability_g_count"])
  ability_g_mean <- mean(ability_g_values)
  ability_g_sd <- sd(ability_g_values)
  
  return (c(
    id=logfile,
    attack_mean=attack_mean,
    attack_sd=attack_sd,
    defense_mean=defense_mean,
    defense_sd=defense_sd,
    ability_g_mean=ability_g_mean,
    ability_g_sd=ability_g_sd
  ))
})
coeff_means
