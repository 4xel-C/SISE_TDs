# Génération du nombre aléatoire
nombre <- trunc(runif(1, 1, 1000))

essais <- 0
n <- 0

while (n != nombre) {
  
  # Incrémenter les essais
  essais <- essais + 1
  
  # input utilisateur
  print("Choisir un nombre:")
  n <- scan()
  
  # logique
  if (n < nombre) {
    print("Plus petit")
  } else if (n > nombre) {
    print("Plus grand")
  } else {
    print("Vous avez trouvé la solution avec...")
    print(paste0(essais, " essais!"))
  }
  
}
