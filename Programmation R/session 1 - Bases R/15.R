# voyelles
voyelles <- c("A", "E", "I", "O", "U", "Y")

# input utilisateur.
print("Choisir un mot: ")
mot <- scan(what = "character")
mot <- toupper(mot)

result <- 0

for (i in 1:nchar(mot)) {
  if (substr(mot, i, i) %in% voyelles) {
    result  <- result + 1
  }
}


print(paste0("Il ya ", result, " voyelles", " dans le mot ", mot))