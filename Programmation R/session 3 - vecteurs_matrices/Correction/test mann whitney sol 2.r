test_wmw <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) != 2){
    return (NA)
  } else
  {
    #taille du vecteur
    n <- length(x)
    #vecteur de rangs
    rangs <- rank(x)
    #id de la modalité la moins fréquente
    frequence <- table(y)
    id <- which.min(frequence)
    #statistique de test
    W <- sum(rangs[unclass(y)==id])
    #na et nb
    na <- frequence[id]
    nb <- n - na
    #espérance sous H0
    EW <- (na*(na+nb+1))/2
    #variance sous H0
    VW <- (na*nb*(na+nb+1))/12
    #u
    u <- (W-EW)/sqrt(VW)
    #p-value
    p <- 2*pnorm(abs(u),lower.tail=FALSE)
    #renvoyer le tout
    return(list(u=u,pvalue=p))
  }
}

#données pour tester la fonction
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor(c("h","f","f","h","f","f","f","h","h","f"))
#tester la fonction
print(test_wmw(x,y))