# Input utilisateur
print("n = ")
n <- abs(scan())

# Verification des diviseurs

is.prime <- TRUE

for (i in 2:(n-1)) {
  if (n %% i == 0) {
    is.prime <- FALSE
    break
  }
}

if (is.prime) {
  print(paste0("Le nombre ", n, " est premier"))
} else {
  print(paste0("Le nombre ", n, " n'est pas premier et est divisible par ", i))
}

