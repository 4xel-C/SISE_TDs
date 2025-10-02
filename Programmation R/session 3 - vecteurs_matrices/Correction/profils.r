#fonction divisant un vecteur par sa somme - renvoie un vecteur
div.sum <- function(x){
  s <- sum(x)
  return(x/s)
}

#calcule le profil colonne d'un tableau de contingence
profil.colonne <- function(m){
  #appliquer div.sum sur chaque colonne de m
  z <- apply(m,2,div.sum)
  #z est bien une matrice
  return(z)
}

#calcule le profil ligne d'un tableau de contingence
profil.ligne <- function(m){
  #appliquer div.sum pour chaque ligne de m
  z <- apply(m,1,div.sum)
  return(t(z)) #bizarrerie de R, on doit transposer ici
}

m <- matrix(c(33,94,21,63,452,115,4,13,5,8,154,38),nrow=3,ncol=4)
print(m)

print(profil.colonne(m))

print(profil.ligne(m))