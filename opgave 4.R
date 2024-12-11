#4.1

# Indlæs data fra CSV : https://sdg.statistikbank.dk/statbank5a/SelectVarVal/Define.asp?Maintable=FU02&PLanguage=0
alkodats <- read.csv("FU02.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, strip.white=TRUE, na.strings=c(NA,""), skip = 3)
typeof(alkodats)
class(alkodats)
str(alkodats)

colnames(alkodats)
colnames(alkodats) <- trimws(colnames(alkodats))  # Fjerner hvide mellemrum
colnames(alkodats)
# Filtrér data fra 2000 til 2022
alkodats <- alkodats[2:11,c(2,9:31)]
# Giv kolonnerne meningsfulde navne
colnames(alkodats)[1] <- "Forbrugsgruppe"
# Ret forbrugsgrupperne ved hjælp af rækkeindeks
#alkodats$Forbrugsgruppe[1] <- "spiritus og likoer"
#alkodats$Forbrugsgruppe[2] <- "Alkoholiske Laeskedrikke"
#alkodats$Forbrugsgruppe[3] <- "Vin af Druer"
#alkodats$Forbrugsgruppe[4] <- "Vin af Andre Frugter"
#alkodats$Forbrugsgruppe[5] <- "Hedvin"
#alkodats$Forbrugsgruppe[6] <- "vinbaseret laeskedrik"
#alkodats$Forbrugsgruppe[7] <- "Pils og guldbajer"
#alkodats$Forbrugsgruppe[8] <- "Andre alkoholholdige bajer"
#alkodats$Forbrugsgruppe[9] <- "bajere med lavt alkoholindhold og alkoholfri bajere"
#alkodats$Forbrugsgruppe[10] <- "bajer-baserede drikkevarer"

#write.csv(alkodats,"destill.csv", row.names = TRUE)
#skriv den rene fil som csv


# Læs ren csv
destill <- read.csv("destill.csv", encoding = "ISO-8859-1")
destill <- destill[-c(2,4,5,6,10),] 

# Længere dataformat
library(tidyr)
long_data <- gather(destill, key = "Year", value = "Value", X2000:X2022)

# Fjern "X" fra årstallene
long_data$Year <- as.numeric(sub("X", "", long_data$Year))

# Plot med ggplot2 og tilpas x-aksen
library(ggplot2)
ggplot(long_data, aes(x = Year, y = Value, color = Forbrugsgruppe, group = Forbrugsgruppe)) +
  geom_line() +
  labs(title = "Udvikling i alkoholforbrug over tid", x = "År", y = "Forbrug i mio.DKK", caption = "Kilde: dst") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Vinklede labels under x-aksen
  scale_color_discrete(name = "Forbrugsgrupper") +
  guides(color = guide_legend(ncol = 2))  # Fordel legenden over to kolonner




#4.2

# Fjern den første kolonne
numdestill <- destill[,-1]

# Transponer dataframen
numdestillt <- as.data.frame(t(numdestill))

# Opret en vektor med forbrugsgrupperne (som matcher antallet af kolonner)
forbrugsgrupper <- c("spiritus og likoer", "Vin af Druer", 
                     "Pils og guldbajer", "Andre alkoholholdige bajer", 
                     "bajere med lavt alkoholindhold og alkoholfri bajere")


#forbrugsgrupper <- c("spiritus og likoer", "Alkoholiske Laeskedrikke", "Vin af Druer", 
#"Vin af Andre Frugter", "Hedvin", "vinbaseret laeskedrik", 
#"Pils og guldbajer", "Andre alkoholholdige bajer", 
#"bajere med lavt alkoholindhold og alkoholfri bajere", 
#"bajer-baserede drikkevarer")



# Brug forbrugsgrupper som kolonnenavne
colnames(numdestillt) <- forbrugsgrupper[]

# Konverter alle kolonner til numeriske værdier
numdestillt <- as.data.frame(lapply(numdestillt, function(x) as.numeric(as.character(x))))
nu
# Tjek om der er NA-værdier
print(sum(is.na(numdestillt)))  # Udskriv antallet af NA'er

# Fjern eller håndter eventuelle NA-værdier (du kan fjerne rækker med NA'er eller erstatte dem)
numdestillt_clean <- na.omit(numdestillt)  # Fjerner rækker med NA'er

# Beregn korrelationsmatrixen ved at ignorere rækker med manglende værdier
cor_matrix <- cor(numdestillt, use = "complete.obs")

# Visualisér korrelationsmatrixen
library(corrplot)
corrplot(cor_matrix, method = "circle",
         tl.cex = 0.6,
         addCoef.col = "gray",
         number.cex = 0.7)

print(cor_matrix)


#4.3
#vil folk overhovedet fortælle ærligt hvor meget de drikker(tabu)
#hvordan man drikker - en flaske vin til maden vs 5000 bajer når man skal på old irish
#vil pværdi lille hvis man tænker på folk der drikker sig i hegnet har fået banket "hvis man kun holder sig til en alkohol får man ikke tømmermænd dagen efter" ind i hovedet
#

