print("Prix hors taxe:")

# Input utilisateur
pht <- scan()

# Application taxe
pht <- pht * 0.20 + pht

# Print terminal
print("Prix avec taxe (20%):")
print(pht)