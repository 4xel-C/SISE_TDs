n <- NA

# input utilisateur
while (is.na(n) | n <= 0 ) {
  print("Choisir n:")
  n <- scan()
}

for (i in 1:n) {
  if (i %% 3 == 0 && i %% 5 == 0) {
    print(paste0("FizzBuzz", " ", i))
  } else if (i %% 3 == 0) {
    print(paste0("Fizz", " ", i))
  } else if (i %% 5 == 0) {
    print(paste0("Buzz", " ", i))
  }
}
