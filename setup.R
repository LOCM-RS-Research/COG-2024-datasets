###
# Shared definitions
#

#install.packages(c("jsonlite", "parallel", "GA", "adana", ggplot2", "dplyr", "tidyr", "caret"))

library(jsonlite)
library(parallel)
library(GA)
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)

NUM_HAND_CARDS <- 30

# reference: https://lev.pm/posts/2018-09-01-legends-of-code-and-magic-postmortem/draft, OneLaneIsEnough
tier_list <- c(68, 7, 65, 49, 116, 69, 151, 48, 53, 51, 44, 67, 29, 139, 84, 18, 158, 28, 64, 80, 33, 85,
               32, 147, 103, 37, 54, 52, 50, 99, 23, 87, 66, 81, 148, 88, 150, 121, 82, 95, 115, 133, 152,
               19, 109, 157, 105, 3, 75, 96, 114, 9, 106, 144, 129, 17, 111, 128, 12, 11, 145, 15, 21, 8,
               134, 155, 141, 70, 90, 135, 104, 41, 112, 61, 5, 97, 26, 34, 73,6, 36, 86, 77, 83, 13, 89,
               79, 93, 149, 59, 159, 74, 94, 38, 98, 126, 39, 30, 137, 100, 62, 122, 22, 72, 118, 1, 47, 71,
               4, 91, 27, 56, 119, 101, 45, 16, 146, 58, 120, 142, 127, 25, 108, 132, 40, 14, 76, 125, 102,
               131, 123, 2, 35, 130, 107, 43, 63, 31, 138, 124, 154, 78, 46, 24, 10, 136, 113, 60, 57, 92,
               117, 42, 55, 153, 20, 156, 143, 110, 160, 140)

cardlist_columns <- c("name", "type", "cost", "attack", "defense", "abilities", "playerHP", "enemyHP", "cardDraw", "description")
read_cardlist <- function(filename="cardlist.txt") {
  cardlist <- readLines(filename)
  cardlist <- gsub(" ; ", ";", cardlist)
  cardlist <- read.csv2(
    text = cardlist,
    header = FALSE,
    colClasses = c("integer", "character", "factor", "integer", "integer", "integer", "character", "integer", "integer", "integer", "character"),
    row.names = 1,
    col.names = c("id", cardlist_columns)
  )
  cardlist$ability_b = grepl("B", cardlist$abilities, fixed=TRUE)
  cardlist$ability_c = grepl("C", cardlist$abilities, fixed=TRUE)
  cardlist$ability_d = grepl("D", cardlist$abilities, fixed=TRUE)
  cardlist$ability_g = grepl("G", cardlist$abilities, fixed=TRUE)
  cardlist$ability_l = grepl("L", cardlist$abilities, fixed=TRUE)
  cardlist$ability_w = grepl("W", cardlist$abilities, fixed=TRUE)
  cardlist$abilities <- NULL
  return(cardlist)
}

write_cardlist <- function(cardlist, filename) {
  cardlist$abilities <- paste(
    ifelse(cardlist$ability_b, "B", "-"),
    ifelse(cardlist$ability_c, "C", "-"),
    ifelse(cardlist$ability_d, "D", "-"),
    ifelse(cardlist$ability_g, "G", "-"),
    ifelse(cardlist$ability_l, "L", "-"),
    ifelse(cardlist$ability_w, "W", "-"),
    sep=""
  )
  write.table(cardlist[cardlist_columns], file=filename, sep=" ; ", quote=FALSE, col.names=FALSE)
}

run_simulation <- function(agent1, agent2, deck1, deck2, cardlist_file, log_file) {
  deck1 <- paste(deck1, collapse = ",")
  deck2 <- paste(deck2, collapse = ",")
  
  # fixiere Seed, damit das Spiel deterministisch ist
  runner_config_params <- paste(
    paste("deckPlayer0Ids", deck1, sep = "="),
    paste("deckPlayer1Ids", deck2, sep = "="),
    paste("cardlistPath", paste(getwd(), cardlist_file, sep="/"), sep = "="),
    paste("seed", "42", sep = "=")
  )
  runner_args <- paste("play", "-l", log_file)
  runner_args <- paste(runner_args, "-d", shQuote(runner_config_params))
  if (!is.null(agent1)) {
    runner_args <- paste(runner_args, "-p1", shQuote(agent1))
  }
  if (!is.null(agent2)) {
    runner_args <- paste(runner_args, "-p2", shQuote(agent2))
  }
  locm_jar <- "./LegendsOfCodeAndMagic-1.2.jar"
  java_args <- paste(
    "--add-opens",
    "java.base/java.lang=ALL-UNNAMED",
    "-jar",
    locm_jar
  )
  cmd <- paste("java", java_args, runner_args)
  
  out <- system2("/bin/bash", args = c("-c", shQuote(cmd)), stdout = TRUE)
  # filter logs and warnings
  out <- out[sapply(out, function(l) !startsWith(l, "WARN") && !startsWith(l, "[") && !startsWith(l, "{"))]
  
  agent_1_score <- as.numeric(out[1])
  agent_2_score <- as.numeric(out[2])
  
  if (agent_1_score == -999) {
    stop("Agent 1 was disqualified")
  }
  if (agent_2_score == -999) {
    stop("Agent 2 was disqualified")
  }
  
  health_difference <- max(0, agent_1_score) - max(0, agent_2_score)
  return(health_difference > 0) # True: Agent 1 wins
}

read_log_matches <- function(log_file, n=-1L) {
  json_lines <- readLines(log_file)
  if (n > 0) {
    # read only last 10% to remove errored runs
    json_lines <- tail(json_lines, n * 1.1)
  }
  
  matches <- bind_rows(mapply(function(json_line, index) {
    # FIXME parallelism messes up some lines
    el <- tryCatch(fromJSON(json_line), error=function(e) NULL)

    if (is.null(el) || "failCause" %in% names(el)) {
      return(data.frame())
    }
    
    game_parameters <- strsplit(el$gameParameters, "\n")[[1]]
    game_parameters <- strsplit(game_parameters, "=")
    game_parameters <- as.list(setNames(sapply(game_parameters, "[", 2), sapply(game_parameters, "[", 1)))
    player0_health <- el$scores$`0`
    player1_health <- el$scores$`1`
    
    if (player0_health == -999 || player1_health == -999) {
      return(data.frame())
    }
    
    player0_deck <- sapply(strsplit(game_parameters$deckPlayer0Ids, ","), as.numeric)
    player1_deck <- sapply(strsplit(game_parameters$deckPlayer1Ids, ","), as.numeric)
    player0_health <- max(0, player0_health)
    player1_health <- max(0, player1_health)
    
    return(data.frame(
      time=c(index, index),
      deck=I(list(player0_deck, player1_deck)),
      enemy_deck=I(list(player1_deck, player0_deck)),
      health_difference=c(player0_health - player1_health, player1_health - player0_health),
      victory=c(player0_health > player1_health, player1_health > player0_health)
    ))
  }, json_lines, seq_along(json_lines), SIMPLIFY=FALSE))
  
  if (n > 0) {
    matches <- tail(matches, n)
  }
  
  return(matches)
}
