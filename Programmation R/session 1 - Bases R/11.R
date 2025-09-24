# Input utilisateur
print("n = ")
n <- abs(scan())

# Verification des diviseurs

diviseurs <- c()

for (i in 1:(n-1)) {
  if (n %% i == 0) {
    diviseurs <- c(diviseurs, i)
  }
}

if (n == sum(diviseurs)) {
  print(paste0("Le nombre ", n, " est parfait!"))
} else { 
  print(paste0("Le nombre ", n, " n'est pas parfait!"))
  print(paste0("Somme diviseurs = ", sum(diviseurs)))
}
