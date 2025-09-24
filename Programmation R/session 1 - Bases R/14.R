# input utilisateur

print("Entrer un mot.")
mot <- scan(what = "character")
mot <- toupper(mot)

print("Entrer une lettre.")
lettre <- scan(what = "character")
lettre <- toupper(lettre)

result <- 0

# Comptage
for (i in 1:nchar(mot)) {
  
  if (substr(mot, i, i) == lettre) {
    result <- result + 1
  }
}

print(paste0("Il ya ", result, " fois la lettre ", lettre, " dans le mot ", mot))
