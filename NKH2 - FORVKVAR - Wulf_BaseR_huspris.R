library(dkstat)
#samtlige tables
tables1 <- dst_get_tables()
#grep til at hente specifikt fra table
subcell <- grepl("husholdning",tables1$text,ignore.case = T)
#subcell2 se hvor smart. Tabel over textindikatorer på tables1, under query på "husholdning"

#grep er udelukkende positioner.
bing <- grep("husholdning",tables1$text,ignore.case = T)

subcell2 <-tables1[subcell,]
#hente metadata til liste query
metaNKH <- dst_meta("NKHC3",lang="da")
metaNKH$variables
filterNKH <- list(
                  KMDR = "Husholdningers forbrugsudgift på dansk område",
                  PRISENHED = "2010-priser, kædede værdier",
                  SÆSON="Sæsonkorrigeret",
                  Tid = "*"
)

metaNKH$values
metaNKH$values$SÆSON
#hent data med filterlist

NKHC3table1st <- dst_get_data("NKHC3",query = filterNKH,lang="da")
head(NKHC3table1st)
NKH2 <- NKHC3table1st[,-c(1:3)]
head(NKH2)
#fra 2000-01-01. >=. 94 kolonner som jeg gerne vil have det. rbind 95?

NKH2 <- NKH2[NKH2$TID >="2000-01-01",]
head(NKH2)
str(NKH2)
#beregn relativ vækst med udgangpsunkt i at kigge 4 frem i min tæller. dvs. den årlige kvartalvise vækst.
#for loop
NKH2$Realvkst <- NA
#ny tom kolonne med ren NA "realvkst"


for (i in 1:(nrow(NKH2)-4)) {
      calctemp <- ((NKH2[i+4,2]-NKH2[i,2])/NKH2[i,2])*100
      #calctemp beregner realvækst +4. så altså procentvis vækst mellem n og n+4 i kvartalerne
      NKH2[i+4,3] <- calctemp
      #bind for loops temp calc til kolonne #realvkst
}

library(ggplot2)
#ggplot2 (tidyverse)
gg2 <- ggplot(NKH2,aes(TID,Realvkst))
gg2 <- gg2+geom_bar(stat="identity",fill="blue")

print(gg2)

head(NKH2)

# Nu skal vi hente forventninger FORV1
metaforv <- dst_meta("forv1")

metaforv$variables


filterforv <- list(INDIKATOR = "*",
                   Tid = "*")
forv1 <- dst_get_data("forv1", query = filterforv, lang = "da")
View(forv1)

# Data fra 2000-01-01 og frem
forv2 <- forv1[forv1$TID >= "2000-01-01",]
View(forv2)
head(forv2)
str(forv2)

# Plot
ggplot(forv2, aes(x = TID, y = value, color = INDIKATOR))+geom_line()
forv2sub <- forv2$INDIKATOR == "Forbrugertillidsindikatoren"
forv2sub <- forv2[forv2sub,]

ggplot(forv2sub, aes(x = TID, y = value, color = INDIKATOR))+geom_line()

#Reshape Dataframe
forvr <- reshape(forv2,idvar = "INDIKATOR", timevar = "TID", v.names = "value", direction = "wide")
# Transposer Dataframe
forvt <- as.data.frame(t(forvr))
colnamevector <- forvt[1,]
colnames(forvt) <- colnamevector

# Slet indikator linien i forvt
forvt <- forvt[-1,]
# Laver ny kolonne til data
forvt$date <- NA
# Fjerner "value." fra Date tiderne
myrows <- row.names(forvt)
forvt$date <- gsub("value\\.", "",myrows)
# Konverterer vores datoer til as.Date format
forvt$date <- as.Date(forvt$date)
rownames(forvt) <- NULL

# Laver ny dataframe som rummer de kvartalvise observationer, bygget som en middelværdi/loop der gennem månedsværdierne beregner mean-værdierne af 3.
forvkvar <- as.data.frame(matrix(nrow = 95, ncol = 14))
View(forvkvar)

# Modificere dataframe
nameskvar <- colnames(forvt)
colnames(forvkvar) <- nameskvar

# Starter med at lave en lille vektor, som skal give mig antal rækker jeg skal bruge i mit nye dataframe
# Laver en talrække 3,6,9,12.... kvartaler (vi bruger denne til at tage index i den gamle)
forvseq <- seq(3, by = 3, length.out = 95)
#forkvar$date propper manuel dato ind, for så at ændre den til as.Date format
forvkvar$date <- 2000-03-01
forvkvar$date <- as.Date(forvkvar$date)
# Laver et loop for ovenstående
for (i in 1:95) {
  #i = 2
  # Snupper datoen fra den gamle dataframe
  tempidx <- forvseq[i]
  forvkvar[i,"date"] <- as.Date(forvt[tempidx,"date"])
  for (j in 1:13) {
  tempmean <- mean(forvt2[tempidx-2,j],forvt2[tempidx-1,j],forvt2[tempidx,j])
  forvkvar[i,j] <- tempmean
  }
  #temp <- mean(forvt2[tempidx-2,1],forvt[tempidx-1,1],forvt[tempidx,1])
  #forvkvar[2,14] <- tempval
  # Kontrol af koden
  #forvt[tempidx, "date"]
  #class(  forvt[tempidx, "date"])
  #forvkvar[i,"date"] <- "2000-03-01"
}
# Laver alle kolonner om til numeric udover den sidste kolonne
forvt2 <- forvt
for (col in 1:(ncol(forvt2) - 1)) {
forvt2[, col] <- as.numeric(forvt2[, col])  
}


#Transposer dataframe
forkvarlong <- as.data.frame(t(forvkvar))
# Laver plot for ovenstående
library(ggplot2)

ggplot(forvkvar, aes(x = date, y = Forbrugertillidsindikatoren)) +
  geom_bar(data = NKH2, aes(x = TID, y = Realvkst, fill = "blue"), stat = "identity")+
  geom_line()

#Opgave fra undervisning
forvkvar3 <- forvkvar[c(-95),]

FTI <- cbind(forvkvar3,NKH2)

z<-if else (x>0,1,0) = ny matrix #Cbind z til til det andet
jn <- ifelse(FTI>0,1,0)
FTI_ny <- cbind(FTI,jn)