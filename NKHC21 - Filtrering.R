library("devtools")
library(dkstat)

#Indhentning af data
NKHC21_meta <- dst_meta(table = "NKHC21", lang = "da")
NKHC21_meta$variables
#Filtrering af information som skal indhentes
NKHC21_filter <- list(
  FORMAAAL = "*",
  PRISENHED = "2010-priser, kædede værdier",
  SÆSON = "*",
  Tid = "*"
)

#Henter dataen, så vi får en DF på dette
NKHC21_data <- dst_get_data(table = "NKHC21", query = NKHC21_filter, lang = "da")

