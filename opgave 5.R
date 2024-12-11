##############opgave 5

#5.1

# Opret kolonnen 'Klasse' med 9 A'er, 9 B'er, 9 C'er og 9 D'er
klasse <- rep(c("A", "B", "C", "D"), each = 9)

# Opret kolonnen 'Uge' med gentagelsen af tallene 1 til 9 for hver klasse
uge <- rep(seq(1, 9), times = 4)

# Opret kolonnen 'Score' med vilkårlige værdier (her bruger vi tal fra 50 til 100 som eksempel)
score <- sample(c(-3,02,4,7,10,12), 36, replace = TRUE)

# Sæt det hele sammen i en dataframe
klassedf <- data.frame(Klasse = klasse, Uge = uge, score = score)

# Tjek dataframen
print(klassedf)
summary(klassedf)

#5.2 - SPØRG LIGE DE ANDRE OM DERES ER BEDRE
# Opgave 5.2 - Opret en tom dataframe til resultatet
nyklassedf <- data.frame(Klasse = character(), Uge = integer(), Score = numeric())

# Loop igennem alle rækker i den oprindelige dataframe
for (i in 1:nrow(klassedf)) {
  
  # Brug modulo-operatoren til at finde hver tredje række #a%%b=a-([a/b]*b) - kun tager hele tal og ikke decimal
  if (i %% 3 == 0) {
    
    # Hent "Klasse" og "Uge" fra den nuværende række
    ny_klasse <- klassedf$Klasse[i]
    ny_uge <- klassedf$Uge[i]
    
    # Beregn gennemsnittet af de tre seneste observationer i "score"
    gennemsnit_score <- mean(klassedf$score[(i-2):i])
    
    # Tilføj en ny række til den nye dataframe
    nyklassedf <- rbind(nyklassedf, data.frame(Klasse = ny_klasse, Uge = ny_uge, Score = gennemsnit_score))
  }
}

# Udskriv den nye 9x3 dataframe
print(nyklassedf)


#5.3 - spørg i klassen

# Opgave 5.2 - Den allerede oprettede dataframe 'nyklassedf'
# (her bruger vi samme logik som i tidligere kode)

# Installer og indlæs tidyr-pakken, hvis du ikke allerede har den
#install.packages("tidyr")
library(tidyr)

# Konverter den eksisterende dataframe ved hjælp af pivot_wider
nyklassedf_wide <- nyklassedf %>%
  pivot_wider(names_from = Klasse, values_from = Score)

# Tjek den nye dataframe med kolonnerne "Uge", "A", "B", "C", "D"
print(nyklassedf_wide)

