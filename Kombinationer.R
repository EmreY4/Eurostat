combn(FTI[1,2:ncol(FTI)], 1)

combn(FTI[1,2:ncol(FTI)], 1)

#Baums måde at gøre det på
a <- matrix(1:60, nrow = 5, ncol = 12)
a_comb1 <- combn(a[2:5,1:ncol(a)], 1)

# Data frame (Prøv at skifte sidste tal for at ændre tidsperioder) Meningen er at få perioder for forskellige tidsperioder
a_comb1 <- data.frame(combn(a[1:ncol(a)], 3))

#Tag udsnit ud af den
length(a_comb1)
dim(a_comb1)
a_comb1[2.1:70]

combn(a[1,1:ncol(a)], 1)

#
combn(FTI[1:5,2:ncol(FTI)], 1)

combn(FTI[1:5,2:ncol(FTI)], 5)

#
combn(a_comb2[1:5,2:ncol(a_comb2)], 1)

combn(a_comb2[1:5,2:ncol(a_comb2)], 5)

#
forvkvartest2 <- matrix(as.numeric(forvkvar[1,2:5]), ncol = 4)
colMeans(combn(forvkvartest2[,1:ncol(forvkvartest2)],4))

#
combn(forvkvartest2[1,1:ncol(forvkvartest2)], 3)

# Loop
#forvkvartest2 <- rep(list(rep(list(rep(list())), nrow(c)), ncol(c) - 1)
comb <- rep(list(rep(list(rep(list())),nrow(forvkvartest2))),12)
  for(i in 1:(ncol(forvkvartest2))){
    forvkvartest3 <- list(colMeans(combn(forvkvartest2[,1:ncol(forvkvartest2)],i)))
}

# Samme som ovenstående men ændring af kolonne
forvkvartest2 <- matrix(as.numeric(forvkvar[1:2]), ncol = 4)
colMeans(combn(forvkvartest2[1:ncol(forvkvartest2)],4))

# Loop
#forvkvartest2 <- rep(list(rep(list(rep(list())), nrow(c)), ncol(c) - 1)
comb <- rep(list(rep(list(rep(list())),nrow(forvkvartest2))),12)
  for(i in 1:(ncol(forvkvartest2))){
    forvkvartest4 <- list(colMeans(combn(forvkvartest2[,1:ncol(forvkvartest2)],i)))
  }

