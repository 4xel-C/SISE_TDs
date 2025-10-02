residus.ajustes <- function(m){
  #effectif total
  n <- sum(m)
  #marges ligne et colonne
  sLig <- apply(m,1,sum)
  sCol <- apply(m,2,sum)
  #tableau th�orique (sous ind�pendance)
  mL <- matrix(sLig,nrow=length(sLig),ncol=1)
  mC <- matrix(sCol,nrow=1,ncol=length(sCol))
  e <- (mL %*% mC)/n #produit vectoriel pour avoir la matrice sous ind�p.
  #matrice des r�sidus ajust�s
  res <- m #juste pour dimensionner res � la m�me taille que m
  for (i in 1:nrow(m)){
    for (j in 1:ncol(m)){
      res[i,j] <- (m[i,j]-e[i,j])/sqrt(e[i,j]*(1.0-mL[i]/n)*(1.0-mC[j]/n))
    }
  }
  return(res)
}

m <- matrix(c(33,94,21,63,452,115,4,13,5,8,154,38),nrow=3,ncol=4)
print(m)

print(residus.ajustes(m))
