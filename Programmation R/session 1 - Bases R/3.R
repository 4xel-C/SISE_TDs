# Inputs utilisateur
print("Jours de location: ")
jour <- scan()

print("km (entier)")
km <- scan()

print("code (entier)")
code <- scan()

# Calcul du prix
if (code == 1) {
  prix <- 0.5 * km + 80 * jour
} else if (code == 2) {
  prix <- 0.8 * km + 85 * jour
} else if (code == 3) { 
  prix <- 0.8 * km + 85 * jour
} else { 
  prix <- NA  
}

if (is.na(prix)) {
  print("Mauvais code entré: Le prix n'est pas calculé.")
} else {
  if (jour > 30) {
    prix <- prix/ 2
  }
  print("Prix calculé:")
  print(prix)
}


