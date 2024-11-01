# Loop
a_comb2 <- rep(list(rep(list(rep(list())), nrow(c)), ncol(c) - 1)
               
               for (i in 1:nrow(c)) {
                 for (j in 1:(ncol(c) - 1)) {
                   a_comb2[[j]][[i]] <- colMeans(combn(c[i, ], j))
                 }
               }
               
               
               
               
               
               # CHAT Initialize a_comb2 as a list of lists
               a_comb2 <- rep(list(rep(list(rep(list(), ncol(a_comb2))), nrow(a_comb2)), ncol(a_comb2))
                              # Loop through the combinations
                              for (j in 1:(ncol(a_comb2))) {
                                for (i in 1:nrow(a_comb2)) {
                                  # Calculate column means for different combinations of columns
                                  a_comb2[[j]][[i]] <- colMeans(combn(a_comb2[i,], j))
                                }
                              }
                              
                              
                              
                              

#Loop
a_comb2 <- a_comb1
a_comb2 <- rep(list(rep(list(rep(list())), nrow(a_comb2), ncol(a_comb2)))
               for (j in 1:(ncol(a_comb2))) {
                 a_comb2[[j]][[i]] <- colMeans(combn(a_comb2[i,2:ncol(a_comb2)], j))
               } 
               #
               a_comb2 <- rep(list(rep(list(rep(list())), nrow(a_comb2))), ncol(a_comb2)-1))
for (j in 1:(ncol(a_comb2)-1)) {
  for (i in 1:length(a_comb2[[1]])) {
    numeric_data <- as.numeric(a_comb2[i,2:ncol(a_comb2)])
    a_comb2[[j]][[i]] <- colMeans(combn(a_comb2[i,2:ncol(a_comb2)], j))
  } 
}


# Select the relevant columns
questions_data <- FTI[, ]

#
combn(questions_data[2:5, ncol(questions_data)], 3))

# Create all combinations using combn
all_combinations <- combn(questions_data, 1:18, simplify = FALSE)

# View the combinations
for (i in 1:length(all_combinations)) {
  print(all_combinations[[i]])
}
#

# Create an empty list to store combinations
all_combinations <- list()

# Generate combinations for each length from 1 to 18
for (k in 1:18) {
  combinations_k <- combn(questions_data, k, simplify = FALSE)
  all_combinations[[as.character(k)]] <- combinations_k
}

#