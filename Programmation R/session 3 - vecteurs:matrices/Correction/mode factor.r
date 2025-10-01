freqmax <- function(v){
 if (is.factor(v) == FALSE){
  return(NA)
 }
 #fr�quence absolue
 t <- table(v)
 #indice de l'�l�ment
 i <- which.max(t)
 return(list(modalite=levels(v)[i],valeur=max(t)))
}

#cr�er le vecteur de type factor
v <- c(1,2,1,1,1,2,1)
sexe <- factor(v)
levels(sexe) <- c("homme","femme")
print(sexe)

#appliquer la fonction
print(freqmax(sexe))


