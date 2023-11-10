###
# GA implementation
#

options(error=function() {
  traceback(2);
  if(!interactive()) quit("no", status = 1, runLast = FALSE)
})

source("setup.R")
source("model_features.R")

NUM_CARDS <- 160

# =============================================================================
#  Utils
# =============================================================================

# https://stat.ethz.ch/pipermail/r-help/2006-May/106278.html
saveSubplot <- function() {
  if (!exists("subplotPars", mode="list"))
    subplotPars <<- list();
  p <- par(no.readonly=TRUE);
  mfg <- p$mfg;
  key <- mfg[1]*(mfg[3]-1)+mfg[2];
  subplotPars[[key]] <<- p;
  invisible(key);
}

restoreSubplot <- function(mfg) {
  opar <- par();
  if (length(mfg) == 2)
    mfg <- c(mfg, par("mfg")[3:4]);
  key <- mfg[1]*(mfg[3]-1)+mfg[2];
  p <- subplotPars[[key]];
  # Move 'mfg' last
  mfg <- p$mfg;
  p$mfg <- NULL;
  p$mfg <- mfg;
  par(p);
  invisible(opar);
}

plot_barline <- function(counts, iter, max_iter, title, counts_sum) {
  if (iter == 1) {
    plot(1, type = "n",
         xlab = "Iteration",
         ylab = title,
         xlim = c(1, max_iter),
         ylim = c(0, counts_sum),
         yaxt = "n",
         bty = "n"
    )
  }
  
  dots <- c(0.0, as.numeric(cumsum(counts)))
  for (i in seq(1, length(dots) - 1)) {
    y <- c(dots[i], dots[i + 1])
    lines(x = c(iter, iter), y = y, col = i, lwd = 4)
    if (iter == 2) {
      text(x = iter, y = sum(y) / 2, names(counts)[i])
    }
  }
}

create_combinations <- function(N, K) {
  # balanced k-random-opponents according to Panait and Luke, 2002
  while (TRUE) {
    population <- 1:N
    games_played <- rep(0, N)
    opponents_played <- vector("list", N)
    results <- matrix(nrow = 0, ncol = 2)
    
    while (length(population) > 0) {
      individual <- population[1]
      opponents <- setdiff(population, c(individual, opponents_played[[individual]]))
      
      if (length(opponents) == 0) {
        opponents <- setdiff(1:N, c(individual, opponents_played[[individual]]))
      }
      
      if (length(opponents) == 0) {
        # balancing impossible, restart
        break
      }
    
      opponent <- sample(opponents, 1)
      results <- rbind(results, c(individual, opponent))
      
      games_played[individual] <- games_played[individual] + 1
      games_played[opponent] <- games_played[opponent] + 1
      opponents_played[[individual]] <- c(opponents_played[[individual]], opponent)
      opponents_played[[opponent]] <- c(opponents_played[[opponent]], individual)
      
      if (games_played[individual] == K) {
        population <- setdiff(population, individual)
      }
      if (games_played[opponent] == K) {
        population <- setdiff(population, opponent)
      }
    }
    if (length(population) == 0) {
      break
    }
  }
  return(results)
}
# table(c(create_combinations(200, 20)))

log_change <- function(iteration, old_genotype, new_genotype, type) {
  old_genotype_split <- split_genotype(old_genotype)
  new_genotype_split <- split_genotype(new_genotype)
  changes_count <- sum(xor(old_genotype_split$card, new_genotype_split$card)) / 2
  if (changes_count > 0) {
    old_phenotype <- genotype_to_phenotype(old_genotype)
    new_phenotype <- genotype_to_phenotype(new_genotype)
    change_entry <- data.frame(
      iteration=iteration,
      previous_cards=I(list(old_phenotype$card)),
      cards=I(list(new_phenotype$card)),
      rs_used=new_phenotype$rs_index,
      type=type,
      changes_count=changes_count
    )
    write.table(change_entry, file=gsub(".njson", "_changes.csv", LOG_FILE, fixed=TRUE), append=TRUE, sep=";", col.names=FALSE, quote=TRUE)
  }
}

# =============================================================================
#  fitness function
# =============================================================================

run_tournament <- function(deck1, deck2, agent, cardlist_file) {
  agent_bin <- NULL
  #agent_bin <- ifelse(agent == "expert", "./Coac/main", "python3 random_agent.py")
  #agent_bin <- "python3 random_agent.py" # 0.7s per game
  #agent_bin <- "./Coac/main" # 1.5-2.0s per game
  #agent_bin <- "./Chad/agent/target/debug/agent" # 10-15s per game
  
  winner <- NULL
  if (all(deck1 == deck2)) {
    winner <- sample(1:2, 1)
  }
  
  while (is.null(winner)) {
    tryCatch(winner <- run_simulation(agent_bin, agent_bin, deck1, deck2, cardlist_file, LOG_FILE), error = function(cond) {
      print(cond)
    })
  }

  return(ifelse(winner, 1, 2))
}

run_k_random_opponents <- function(population, agent, cardlist_file, k) {
  popSize <- nrow(population)
  # let each individual play the same number of games
  matchups <- create_combinations(popSize, k)

  winners <- parApply(clust, matchups, 1, function(matchup, population) {
    deck1 <- genotype_to_phenotype(population[matchup[1], ])$card
    deck2 <- genotype_to_phenotype(population[matchup[2], ])$card
    winner <- run_tournament(deck1, deck2, agent, cardlist_file)
    return(matchup[winner])
  }, population = population)
  
  return(list(matchups=matchups, winners=winners))
}

calculate_fitness <- function(results, popSize) {
  # fitness = number of wins / number of games per individual
  win_table <- table(factor(results$winners, levels = 1:popSize))
  play_table <- table(factor(c(results$matchups), levels = 1:popSize))
  fitness <- as.numeric(win_table / play_table)
  return(fitness)
}

# =============================================================================
#  Encoding and population
# =============================================================================

split_genotype <- function(genotype) {
  if (length(genotype) == NUM_CARDS) {
    rs_genotype <- NULL
  } else {
    rs_genotype <- genotype[1:(length(genotype) - NUM_CARDS)]
  }
  card_genotype <- genotype[(length(genotype) - NUM_CARDS + 1):length(genotype)]
  return(list(
    rs=rs_genotype,
    card=card_genotype
  ))
}

genotype_to_phenotype <- function(genotype) {
  assert_genotype_valid(genotype)
  split <- split_genotype(genotype)
  
  if (is.null(split$rs) || sum(split$rs) == 0) {
    rs_index <- 0
  } else {
    rs_index <- min(which(split$rs == 1))
  }
  
  return(list(
    rs_index=rs_index,
    cards=which(split$card %in% 1)
  ))  
}

assert_genotype_valid <- function(genotype, id = "") {
  split <- split_genotype(genotype)

  if (!is.null(split$rs) && sum(split$rs) > 1) {
    stop(paste(id, "Produced invalid genotype, expected 0 or 1 1-bit for RS but found", sum(split$rs), "1-bits"))
  }
  
  if (length(split$card) != NUM_CARDS) {
    stop(paste(id, "Produced invalid genotype, expected", NUM_CARDS, "cards but found", length(split$card), "cards"))
  }
  if (sum(split$card) != NUM_HAND_CARDS) {
    stop(paste(id, "Produced invalid genotype, expected", NUM_HAND_CARDS, "1-bits for cards but found", sum(split$card), "1-bits"))
  }
}

generate_random_individual <- function(rs_count, rs_index) {
  individual_cards <- sample(c(rep(0, NUM_CARDS - NUM_HAND_CARDS), rep(1, NUM_HAND_CARDS)))

  if (rs_count == 0) {
    individual_rs <- c()
  } else {
    individual_rs <- rep(0, rs_count)
    individual_rs[rs_index] <- 1
  }
  return(c(individual_rs, individual_cards))
}

generate_random_population <- function(rs_count, popSize, rs_user_fraction) {
  individuals <- sapply(1:popSize, function(x) {
    if (x <= (1 - rs_user_fraction) * popSize) {
      generate_random_individual(rs_count, 0)
    } else {
      generate_random_individual(rs_count, ((x-1) %% rs_count) + 1)
    }
  })
  population <- matrix(individuals, nrow = popSize, byrow = TRUE)
  return(population)
}

# =============================================================================
#  Crossover
# =============================================================================

count_preserving_crossover <- function(parent_genotypes, pcrossover) {
  # swap random number of cards
  
  parents1_split <- split_genotype(parent_genotypes[1, ])
  parents2_split <- split_genotype(parent_genotypes[2, ])
  
  child1_cards <- parents1_split$card
  child2_cards <- parents2_split$card
  
  # only swap cards not in common by parents
  cards1 <- which(child1_cards == 1)
  cards2 <- which(child2_cards == 1)
  cards1_missing_in_cards2 <- setdiff(cards1, cards2)
  cards2_missing_in_cards1 <- setdiff(cards2, cards1)
  
  desired_swaps <- rbinom(1, NUM_HAND_CARDS, pcrossover)
  
  number_of_cards_to_swap <- min(length(cards1_missing_in_cards2), length(cards2_missing_in_cards1), desired_swaps)
  if (number_of_cards_to_swap == 0) {
    out <- list(children = parent_genotypes, fitness = rep(NA, 2))
    return(out)
  }

  card_1_to_swap <- sample(cards1_missing_in_cards2, number_of_cards_to_swap)
  card_2_to_swap <- sample(cards2_missing_in_cards1, number_of_cards_to_swap)
  
  child1_cards[card_1_to_swap] <- 0
  child1_cards[card_2_to_swap] <- 1
  child2_cards[card_1_to_swap] <- 1
  child2_cards[card_2_to_swap] <- 0

  # double-check the result
  assert_genotype_valid(child1_cards)
  assert_genotype_valid(child2_cards)
  
  changes1_count <- sum(xor(child1_cards, parents1_split$card)) / 2
  if (changes1_count > number_of_cards_to_swap) {
    stop(paste("Produced invalid crossover for child 1, changed", changes1_count, "bits but expected to change", number_of_cards_to_swap, "bits"))
  }
  
  changes2_count <- sum(xor(child2_cards, parents2_split$card)) / 2
  if (changes2_count > number_of_cards_to_swap) {
    stop(paste("Produced invalid crossover for child 2, changed", changes2_count, "bits but expected to change", number_of_cards_to_swap, "bits"))
  }
  
  children_cards <- matrix(c(child1_cards, child2_cards), byrow=TRUE, nrow=2)
  # children get the RS by the parent with which they have most cards in common
  children_rs <- matrix(c(parents1_split$rs, parents2_split$rs), byrow=TRUE, nrow=2)
  
  children_genotypes <- cbind(children_rs, children_cards)
  
  return(list(children = children_genotypes, fitness = rep(NA, 2)))
}

#while (TRUE) { count_preserving_crossover(generate_random_population(2, 2, 1.0), sample.int(100, 1)/100) }
#crossover_debug <- generate_random_population(2, 2, 1.0)
#crossover_debug_children <- count_preserving_crossover(crossover_debug, 0.2)$children
#sum(xor(crossover_debug[1, ], crossover_debug_children[1, ]))

# =============================================================================
#  mutation
# =============================================================================

count_preserving_mutation <- function(parent_genotype, pmutation) {
  parent_split <- split_genotype(parent_genotype)
  
  # swap random cards
  number_of_cards_to_swap <- rbinom(1, NUM_HAND_CARDS, pmutation)
  
  if (number_of_cards_to_swap == 0) {
    return(parent_genotype)
  }
  
  cards <- which(parent_split$card %in% 1)
  not_cards <- which(parent_split$card %in% 0)
  swap_for_0 <- sample(cards, number_of_cards_to_swap)
  swap_for_1 <- sample(not_cards, number_of_cards_to_swap)
  
  child_card <- parent_split$card
  child_card[swap_for_0] <- 0
  child_card[swap_for_1] <- 1

  # child get the RS of its parent
  child_rs <- parent_split$rs
  
  child_genotype <- c(child_rs, child_card)
  
  return(child_genotype)
}

#mutation_debug <- generate_random_population(2, 2, 1.0)
#mutation_debug_child <- count_preserving_mutation(mutation_debug, 0.2)
#sum(xor(mutation_debug, mutation_debug_child))

rs_mutation <- function(parent_genotype, rs_recommend, pmutation) {
  # apply RS-mutation with additional checks and logging
  parent_genotype_split <- split_genotype(parent_genotype)
  recommendation_size <- rbinom(1, NUM_HAND_CARDS, pmutation)
  
  if (recommendation_size == 0) {
    return(parent_genotype)
  }
  
  # child gets RS of its parent
  child_genotype_rs <- parent_genotype_split$rs
  
  parent_phenotype <- genotype_to_phenotype(parent_genotype)
  
  child_cards <- rs_recommend(parent_phenotype$cards, recommendation_size)
  child_genotype_cards <- rep(0, NUM_CARDS)
  child_genotype_cards[child_cards] <- 1
  
  child_genotype <- c(child_genotype_rs, child_genotype_cards)
  assert_genotype_valid(child_genotype)

  changes_count <- sum(xor(child_genotype_cards, parent_genotype_split$card)) / 2
  if (changes_count > recommendation_size) {
    stop(paste("Produced invalid RS mutation, changed", changes_count, "bits but expected to change", recommendation_size, "bits"))
  }

  print(paste("RS mutation with", parent_phenotype$rs_index, "changed", changes_count, "expected", recommendation_size))
  
  return(child_genotype)
}

# =============================================================================
#  Monitor
# =============================================================================

population_monitor <- function(object) {
  number_of_unique_decks <- nrow(unique.matrix(object@population))
  card_occurences <- colSums(object@population)
  number_of_unique_cards <- sum(card_occurences > 0)
  print(paste(
    "Iteration",
    object@iter,
    "unique decks:",
    paste(number_of_unique_decks, nrow(object@population), sep = "/"),
    "unique cards:",
    paste(number_of_unique_cards, ncol(object@population), sep = "/")
  ))
  
  rs_count <- object@nBits - NUM_CARDS
  if (rs_count > 0) {
    used_rs <- as.vector(apply(object@population, 1, function(genotype) genotype_to_phenotype(genotype)$rs_index))
    rs_dist <- table(used_rs)
    print(rs_dist)
  }
}

create_log_entries <- function(results, object) {
  iteration <- rep(object@iter, object@popSize)
  cards_list <- I(as.list(as.data.frame(apply(object@population, 1, function(genotype) genotype_to_phenotype(genotype)$card))))
  rs_used <- apply(object@population, 1, function(genotype) genotype_to_phenotype(genotype)$rs_index)

  log <- data.frame(
    iteration = iteration,
    cards = cards_list,
    rs_used = rs_used,
    plays = sapply(1:object@popSize, function(i) sum(results$matchups == i)),
    wins = sapply(1:object@popSize, function(i) sum(results$winners == i)),
    fitness = object@fitness
  )
  rownames(log) <- paste(iteration, 1:object@popSize, sep = "_")
  
  return(log)
}

# =============================================================================
#  Main
# =============================================================================

# A run starts 3 processes (game + 2 agents)
clust <- makeCluster(floor(detectCores() * 0.75), outfile = "")
clusterExport(clust, list("run_simulation", "fromJSON", "split_genotype", "genotype_to_phenotype", "assert_genotype_valid", "run_tournament", "NUM_CARDS", "NUM_HAND_CARDS"))
gaControl("useRcpp" = FALSE) # Mac M1 (https://github.com/luca-scr/GA/issues/52)

run_ga <- function(
    k, popSize, maxiter,
    rs_usage_probability=0, rs_recommend_factory=list(), rs_user_fraction,
    pcrossover=0,
    pmutation=1/NUM_HAND_CARDS,
    tournament_k=2, elitism=0,
    decide_cardlist_file_fn
    # pcrossover: probability for an individual to reproduce
    # pmutation: probability of mutation
) {
  log <- data.frame(
    iteration = c(),
    deck = c(),
    rs_used = c(),
    wins = c(),
    plays = c(),
    fitness = c()
  )
  rs_recommend_dict <- list()
  update_rs_models <- function(iteration) {
    rs_recommend_dict <<- lapply(rs_recommend_factory, function(build) build(iteration + 1))
  }
  post_fitness <- function(object, eps=gaControl(object@type)@eps, agent, ...) {
    print(paste("Starting iteration", object@iter))
    results <- run_k_random_opponents(object@population, agent, decide_cardlist_file_fn(object@iter), k)
    print("Completed k-random opponents")
    object@fitness <- calculate_fitness(results, object@popSize)
    print("Calculated fitness")
    log <<- rbind(log, create_log_entries(results, object))
    print("Updated logs")
    update_rs_models(object@iter)
    print("Updated RS models")
    
    # wins per RS for monitoring progress
    log %>%
      group_by(rs_used) %>%
      summarize(
        fitness=mean(fitness),
        plays=sum(plays),
        wins=sum(wins)
      ) %>%
      print
    
    write.csv2(log, file=gsub(".njson", ".csv", LOG_FILE, fixed=TRUE))
    print("Wrote log file")
    
    return(object)
  }

  combined_mutation <- function(object, parent) {
    parent_genotype <- as.vector(object@population[parent, ])
    child_genotype <- tryCatch({
      parent_phenotype <- genotype_to_phenotype(parent_genotype)
      if (parent_phenotype$rs_index > 0 && runif(1) < rs_usage_probability) {
        cg <- rs_mutation(parent_genotype, rs_recommend_dict[[parent_phenotype$rs_index]], pmutation)
        log_change(object@iter, parent_genotype, cg, "RS-mutation")
      } else {
        cg <- count_preserving_mutation(parent_genotype, pmutation)
        log_change(object@iter, parent_genotype, cg, "mutation")
      }
      cg
    }, error=function(cond) {
      print(cond)
      return(parent_genotype)
    })
    return(child_genotype)
  }
  
  combined_crossover <- function(object, parents) {
    parent_genotypes <- object@population[parents, , drop = FALSE]
    child_genotypes <- tryCatch({
      res <- count_preserving_crossover(parent_genotypes, pcrossover)
      log_change(object@iter, parent_genotypes[1, ], res$children[1, ], "crossover")
      log_change(object@iter, parent_genotypes[2, ], res$children[2, ], "crossover")
      res
    }, error=function(cond) {
      print(cond)
      return(list(children = parent_genotypes, fitness = rep(NA, 2)))
    })
    return(child_genotypes)
  }
  
  rs_count <- length(rs_recommend_factory)
  GA <- ga(
    type = "binary",
    fitness = function(x, ...) 0, # implemented in postFitness because the fitness function needs access to the population
    postFitness = post_fitness,
    agent = "random",
    population = function(object) generate_random_population(rs_count, object@popSize, rs_user_fraction),
    nBits = rs_count + NUM_CARDS,
    popSize = popSize,
    maxiter = maxiter,
    selection = function(object) gabin_tourSelection(object, k=tournament_k), #gabin_nlrSelection,
    crossover = combined_crossover,
    pcrossover = 1, # implemented in combined_crossover
    mutation = combined_mutation,
    pmutation = 1, # implemented in combined_mutation
    elitism = elitism,
    monitor = population_monitor
  )
  
  return(list(
    GA=GA,
    log=log
  ))
}

# =============================================================================
#  Simulation with RS
# =============================================================================

rs_factory <- function(model_definition, history_length) {
  # history_length: number of last entries to be read
  return(function(iteration) {
    return(build_rs(read_cardlist(decide_cardlist_file(iteration)), LOG_FILE, history_length, model_definition))
  })
}

decide_cardlist_file <- function(iteration) {
  if (iteration < 20) {
    return("cardlist.txt")
  } else {
    return("logs/cardlist_balance.txt")
  }
}

LOG_FILE <- "simulation_debug_new_1026.njson"
clusterExport(clust, list("LOG_FILE", "decide_cardlist_file"))

k <- 10
popSize <- 100

simulation_log <- run_ga(
  pcrossover=1/NUM_HAND_CARDS*2.5,
  pmutation=1/NUM_HAND_CARDS*2.5,
  rs_usage_probability=0.33, # probablity of an RS-Mutation
  rs_user_fraction=1.0, # fraction of RS users in the population
  # in each iteration, approx. k*popSize/2 games are played
  rs_recommend_factory=list(
    aggregates=rs_factory(aggregates_model, 10000),
    utilities=rs_factory(utilities_model, 4000),
    bagofcards=rs_factory(bagofcards_model, 20000),
    ranking=rs_factory(ranking_model, 1000)
  ),
  tournament_k=2,
  k=k, popSize=popSize,
  maxiter=50,
  elitism=popSize/10,
  decide_cardlist_file_fn=decide_cardlist_file
)

stopCluster(clust)
