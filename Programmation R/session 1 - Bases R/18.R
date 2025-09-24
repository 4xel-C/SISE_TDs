print("Choisir un entier entre 1 et 3999: ")
n <- scan()

if (n < 1 | n > 3999) {
  print("Mauvaise entrée utilisateur.")
} else {
  
  result <- ""
  
  # Ajouter les M
  while (n >= 1000) {
    
    result <- paste0(result, "M")
    n <- n - 1000
    
  }
  
  print(result)
  
  
}