######################OPGAVE3

#3.1
terning <- 1:6  # Definerer en terning med værdierne 1 til 6
antalkast <- 25000 # Bestemmer antallet af kast til 25.000

terninger <- function(){
  smid <- sample(terning, antalkast, replace = TRUE)  
  # Slår med 25.000 terninger og gemmer resultaterne
  femere <- sum(smid==5)# Tæller femmerne
  sandsynlighed <- sum(femere/antalkast) # Beregner sandsynligheden for at slå en 5'er
  return(list(femere = femere, sandsynlighed = sandsynlighed))
}
resultatterninger <- terninger()
print(resultatterninger)


#3.2
library(ggplot2)
terning <- 1:6
sekskast <- 6

seksterninger <- function(n) {
  sumkast <- replicate(n, sum(sample(terning, sekskast, replace = TRUE)))
  return(sumkast)
}

titusindekast <- seksterninger(10000)

ggplot(data.frame(sum = titusindekast), aes(x = sum)) +
  geom_bar() +
  labs(title = "Sum af 6 terninger kastet 10.000 gange", x = "Sum", y = "Frekvens")


#3.3
milkast <- seksterninger(1000000)

ggplot(data.frame(sum = milkast), aes(x = sum)) +
  geom_bar() +
  labs(title = "Sum af 6 terninger kastet 1.000.000 gange", x = "Sum", y = "Frekvens")


#3.4

# Lav en tilfældig række af tallene 1, 2, 3, 5, 6
randomrow <- sample(c(1, 2, 3, 5, 6))
randomrow2 <- sample(c(1, 2, 3, 5, 6))

# Lav en matrix med cbind
matrix_data <- cbind(2:6, randomrow)
print(matrix_data)

