#saisie
print("Nombre n :")
n <- scan()

#conformité saisie
if (n < 4 || n > 20){
  print("Taille de sablier non conforme")
} else
{
  
  #n pair => (n-1) niveaux
  #n impair => n niveaux
  nb_niveaux <- ifelse(n %%2 == 0, n - 1, n)
  
  #départ : nombre d'étoiles initial
  nb_etoiles <- n
  
  #pour chaque ligne du sablier
  for (i in 1:nb_niveaux){
    nb_points <- (n - nb_etoiles)%/% 2
    
    chaine <- paste(rep(".",nb_points),collapse="")
    chaine <- paste(chaine,paste(rep("*",nb_etoiles),collapse=""),sep="")
    chaine <- paste(chaine,paste(rep(".",nb_points),collapse=""),sep="")
    
    print(chaine)
    
    ifelse(i <= nb_niveaux%/%2, nb_etoiles <- nb_etoiles-2, nb_etoiles <- nb_etoiles+2)
  }
}