#fonction test du khi-2
#en entrée 2 facteurs
test.khi2 <- function(y,x){
  #conditions de calcul
  if (is.factor(y) != T || is.factor(x) != T || length(x) != length(y)){
    return(NULL)
  } else
  {
  #table de contingence
  m <- table(y,x)
  #effectif total
  n <- sum(m)
  #marges ligne et colonne
  sLig <- apply(m,1,sum)
  sCol <- apply(m,2,sum)
  #tableau théorique (sous indépendance)
  e <- matrix(0,ncol=ncol(m),nrow=nrow(m))
  for (i in 1:nrow(e)){
    for (j in 1:ncol(e)){
      e[i,j] <- sLig[i]*sCol[j]/n
    }
  }
  #statistique du khi-2
  khi2 <- sum((m-e)^2/e)
  #ddl
  ddl <- (nrow(m)-1)*(ncol(m)-1)
  #p-value
  p <- pchisq(khi2,ddl,lower.tail=FALSE)
  #retour
  return(list(chi2=khi2,ddl=ddl,pvalue=p))
  }
}

#deux vecteurs transformés en facteurs
x <- c(1,2,2,3,2,2,1,1,2,2,1,1,1,1,2)
x <- factor(x)
y <- c(1,2,1,2,1,2,1,1,2,1,2,1,1,2,2)
y <- factor(y)

#la fonction de R
print(chisq.test(x,y))

#notre fonction
print(test.khi2(x,y))


