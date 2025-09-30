# alphabet
alphabet <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

# input utilisateurs
print("Choisir un mot à crypter: ")
mot <- scan(what = "character")
mot <- toupper(mot)

print("Choisir un decalage (entier)")
decalage <- scan()

result <- ""

for (i in 1:nchar(mot)) {
  
  lettre <- substr(mot, i, i)
  
  # Recuperer l'index de la lettre dans l'objet retourné.
  index <- regexpr(lettre, alphabet)[1]

  
  newindex <- (index + decalage) %% 26
  
  result <- paste0(result, substr(alphabet, newindex, newindex))
}

print("Impression du message crypté:")
print(result)