library(quantstrat)
library(ggplot2)
library(reshape2)
library(highcharter)

source("config.R")

pair_formation_symbol_env <- new.env()

quantmod::getSymbols(Symbols = symbols, src = "yahoo", env =pair_formation_symbol_env , index.class = "POSIXct", from = pair_formation_start_date, to = pair_formation_end_date, adjust = TRUE)

for(n in ls(envir=pair_formation_symbol_env)){
  pair_formation_symbol_env[[n]]$daily_return <- exp(diff(log(pair_formation_symbol_env[[n]][,6])))
}

rm(envir = pair_formation_symbol_env, ".getSymbols")
correlation_matrix <- matrix(0, nrow = length(pair_formation_symbol_env), ncol = length(pair_formation_symbol_env))
rownames(correlation_matrix) <- names(pair_formation_symbol_env)
colnames(correlation_matrix) <- names(pair_formation_symbol_env)

correlation_list <- new.env(hash = TRUE)

for(row in 1:nrow(correlation_matrix)) {
  for(col in 1:ncol(correlation_matrix)){
    correlation_matrix[row,col] <- cor(pair_formation_symbol_env[[rownames(correlation_matrix)[row]]]$daily_return, pair_formation_symbol_env[[colnames(correlation_matrix)[col]]]$daily_return, use = "pairwise.complete.obs"
                                       correlation_list[[paste(sort(c(rownames(correlation_matrix)[row],colnames(correlation_matrix)[col])), collapse = ",")]] <- correlation_matrix[row, col]
  }
}

correlation_df <- matrix(ncol = 2, nrow = 0, byrow = FALSE)
colnames(correlation_df) <- c("pair", "correlation")

for(n in ls(envir = correlation_list)){
  correlation_df <- rbind(correlation_df, c(n, as.numeric(correlation_list[[n]])))
}

correlation_df <- as.data.frame(correlation_df, stringsAsFactors = FALSE)
correlation_df[,2]<- as.numeric(correlationo_df[,2])
correlation_df <- correlation_df[correlation_df[2] != 1,]

trading_pairs <- strsplit(correlation_df[sort.list(decreasing = TRUE, correlation_df[,2]),][1:top_n_pair,][,1], ",")
















