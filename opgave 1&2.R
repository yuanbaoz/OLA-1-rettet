df <- read.csv("boligsiden.csv", stringsAsFactors = F)
opgave1.2 <- subset(df, (vej == "tousvej" & vejnr == 106) | (vej == "egevej" & vejnr == 20))
df$pris <- gsub(" kr", "", df$pris)
df <- lapply(df, function(x) gsub("[.]", "", x))
df <- as.data.frame(df)
df <-   df[-c(291,1881,2192),]
#fjerne alle NA fra df
dfc <- df[!is.na(df$pris) & 
            !is.na(df$postnr) & 
            !is.na(df$værelser) & 
            !is.na(df$grund) & 
            !is.na(df$kvmpris) & 
            !is.na(df$mdudg), ]

#for at lave graf til boligsiden

dfc[, c("pris", "værelser", "kvmpris","størrelse", "mdudg", "grund")] <- 
  lapply(dfc[, c("pris", "værelser", "kvmpris","størrelse", "mdudg", "grund")], function(x) as.numeric(as.character(x)))

dfc <- data.frame(dfc[, c("pris", "værelser", "kvmpris","størrelse", "mdudg", "grund")])

#lave linær regrassion for opgave 2,2 og 2,3
lm1 <- lm(dfc$størrelse~dfc$kvmpris)
lm2 <- lm(dfc$værelser~dfc$kvmpris)
lm3 <- lm(dfc$mdudg~dfc$kvmpris)
lm4 <- lm(dfc$pris~dfc$kvmpris)
lm5 <- lm(dfc$grund~dfc$kvmpris)
summary(lm1)
summary(lm4)
summary(lm5)

plot(dfc$størrelse,dfc$kvmpris)
abline(lm1)
cor_matrix <- cor(dfc)
library(corrplot)
corrplot(cor_matrix)
