#
micro <- forvkvar[1:86, ]

micro$micro1 <- rowMeans(micro[, c(2, 3, 12)])
print(micro)

micro$micro1 <- round(micro$micro1, digits = 2)

micro$realvkst <- round(NKH2$Realvkst[1:86], digits = 2)

library(ggplot2)
ggplot(data = micro, aes(x = date, y = micro1)) +
  geom_line() +
  geom_col(data = micro, aes(x = date, y = realvkst, fill="Realvækst"))+
  scale_fill_manual(values = c("Realvækst" = "blue")) +
  labs(title = "Microindikator vs. Realvækst",
       x = "Dato",
       y = "Microindikator") +
  theme_minimal()

# lm regression
modelmicro <- lm(realvkst~micro1,data=micro)

summary(modelmicro)

r_squared <- summary(modelmicro)$r.squared
print(r_squared)

#
micro$CCIM <- rowMeans(micro[, c(2, 3, 6, 9)])
micro$CCIM <- round(micro$CCIM, digits = 2)

library(ggplot2)
ggplot(data = micro, aes(x = date, y = CCIM)) +
  geom_line() +
  geom_col(data = micro, aes(x = date, y = realvkst, fill="Realvækst"))+
  scale_fill_manual(values = c("Realvækst" = "blue")) +
  labs(title = "CCI microindikator vs. Realvækst",
       x = "Dato",
       y = "CCI microindikator") +
  theme_minimal()

# LM CCI
modelCCI <- lm(realvkst~CCIM,data=micro)

summary(modelCCI)

r_squaredCCI <- summary(modelCCI)$r.squared
print(r_squaredCCI)






