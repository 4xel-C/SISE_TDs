# User input
print("n = ")
n <- scan()

print("p = ")
p <- scan()

# Calcul
if (p  > n) {
  print("Pas de calcul possible. (p > n")
} else {
  result <- 0
  
  for (i in p:n) {
    result = result + i
  }

print(paste0("Résultat : ", result))
}
