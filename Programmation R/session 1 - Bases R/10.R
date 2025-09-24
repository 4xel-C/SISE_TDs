# Input utilisateur
print("n = ")
n <- abs(scan())

# Verification des diviseurs

is.prime <- TRUE
diviseurs <- c()

for (i in 2:(n-1)) {
  if (n %% i == 0) {
    is.prime <- FALSE
    diviseurs <- c(diviseurs, i)
  }
}

if (is.prime) {
  print(paste0("Le nombre ", n, " est premier"))
} else {
  print(paste0("Le nombre ", n, " n'est pas premier"))
  print(paste0("Plus petit diviseur = ", min(diviseurs)))
  print(paste0("Plus grand diviseur = ", max(diviseurs)))
}

