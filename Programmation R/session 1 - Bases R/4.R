# Entrée utilisateur

print("a =")
a <- scan()

print("b = ")
b <- scan()

print("c = ")
c <- scan()

# declaration des solutions
r <- NA
r1 <- NA
r2 <- NA


# Calcul des racines
if (a == 0) { 
  if (b != 0) {
    r <- -c / b
  }  else {
    r <- NA
  }
  
} else {
  
  # Calcul du delta
  delta <- b**2 - (4 * a* c)
  
  if (delta < 0) {
    r <- NA
  } else if (delta == 0) {
    r <- -b / (2 * a)
  } else {
    r1 <- (-b - sqrt(delta))/(2 * a)
    r2 <- (-b + sqrt(delta))/(2 * a)
  }
}

# Impression de la solution

# Pas de solutions
if (is.na(r) && is.na(r1) && is.na(r2)) {
  print("Pas de solutions.")

# 1 seule solution
} else if (!is.na(r)) {
  print("1 solution trouvé:")
  print(r)
  
# 2 solutions
} else {
  print("Deux solutions trouvées")
  print(paste0("r1 = ", r1))
  print(paste0("r2 = ", r2))
}