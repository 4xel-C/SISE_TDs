# input utilisateur.
print("n = ")
n <- scan()

if (n > 0) { 
  # boucle d'affichage
  for (i in 1:n) {
    print(i)
  }
} else {
  print("n < 0: erreur!")
}
