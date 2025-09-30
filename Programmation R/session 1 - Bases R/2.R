print("Prix hors taxe:")

# Input utilisateur
pht <- scan()

print("Code: ")
code <- scan()


if (code == 1) {
  pht <- pht * 0.33 + pht
  print("Prix avec taxe (33%:")
  print(pht)
} else { 
    pht <- pht * 0.20 + pht
    print("Prix avec taxe (20%:)")
    print(pht)
  }


