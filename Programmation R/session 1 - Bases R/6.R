# Récupération de l'input utilisateur.
print("n = ")
n <- scan()

# Calcul de n factoriel
result <- 1

for (i in 1:n) {
  result <- result * i
}

# impression du résultat
print(paste0("Impression de ", n, " factoriel: ", result))