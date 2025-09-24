# Génération des allumettes.
allumettes <- trunc(runif(1, 15, 20))


player = TRUE

print("Le joueur commence!")
print("")

while (TRUE) {
  
  print(paste0("Il y a ", allumettes, " allumettes restantes"))
  
  action <- NA
  
  # tour du joueur
  if (player) {
    
    print("Tour du joueur!")
    
    while (is.na(action) | action < 1 | action > 3 | action > allumettes) {
      print("Choisir le nombre d'allumettes voulu")
      action <- scan()
    }
    
    
  # Tour de l'ordinateur
  } else {
    
    print("L'ordinateur joue...")
    
    # Choisir au hasard si pas de possibilités de gagner
    if (allumettes > 4) {
      action <- trunc(runif(1, 1, 3))
      
    # Chosir le nombre correct d'allumettes.
    } else {
      if (allumettes == 4) {
        action <- 3
      } else if (allumettes == 3) {
        action <- 2
      } else {
        action <- 1
      } 
    }
    print(paste0("L'ordinateur enlève ", action, " allumettes"))
    
  }
  
  
  
  # Actualiser le score
  allumettes <- allumettes - action 
  
  if (allumettes == 0) {
    break
  }
  
  # Changement de joueur
  player <- !player
}



print("Jeu terminé!")

if (player) {
  print("L'ordinateur a gagné!")
} else {
  print("Le joueur à gagné!")
}