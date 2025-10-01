mann_whitney <- function(v1,v2){
 #chercher le vecteur le plus court <-> l'effectif le plus faible et le mettre en x
 if (length(v1) < length(v2)){
  x <- v1
  y <- v2
  } else
  {
  y <- v1
  x <- v2
  }
  #effectifs
  nx <- length(x)
  ny <- length(y)
  #concatener les vecteur
  z <-c(x,y)
  #calculer les rangs
  rangs <- rank(z)
  #récupérer la somme des rangs de x - la statistique
  W <- sum(rangs[1:nx])
  #moyenne et variance de la statistique
  EW <- nx*(nx+ny+1)/2
  VW <- nx*ny*(nx+ny+1)/12
  #z-value
  z_value <- (W-EW)/sqrt(VW)
  #p-value, test bilatéral
  p_value <- 2*pnorm(abs(z_value),lower.tail=F)
  #return
  return (list(Stat=W,z=z_value,p=p_value))
}

#exemple de données
A <- c(185,168,179,176)
B <- c(159,170,190,167,160,155)

#appel de la fonction et affichage
print(mann_whitney(A,B))

#pour comparaison
res.r <- wilcox.test(A,B,exact=F,correct=F)
print(res.r)
