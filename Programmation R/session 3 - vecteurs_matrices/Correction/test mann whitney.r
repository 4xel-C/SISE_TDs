test_mann_whitney <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) != 2){
    return (NA)
  } else
  {
    #pour le 1er groupe
    n1 = length(x[unclass(y)==1])
    #pour le second groupe
    n2 = length(x[unclass(y)==2])
    #qui est le plus court ?
    if (n1 < n2){
      court <- x[unclass(y)==1]
      long <- x[unclass(y)==2]
    } else
    {
      court <- x[unclass(y)==2]
      long <- x[unclass(y)==1]
    }
    nc <- length(court)
    nl <- length(long)
    #concatener les vecteur
    z <-c(court,long)
    #calculer les rangs
    rangs <- rank(z)
    print(rangs)
    #r?cup?rer la somme des rangs de x - la statistique
    W <- sum(rangs[1:nc])
    #moyenne et variance de la statistique
    EW <- nc*(nc+nl+1)/2
    VW <- nc*nl*(nc+nl+1)/12
    #z-value
    z_value <- (W-EW)/sqrt(VW)
    #p-value, test bilat?ral
    p_value <- 2*pnorm(abs(z_value),lower.tail=F)
    #return
    return (list(Stat=W,z=z_value,p=p_value))
  }
}

#données pour tester la fonction
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor(c("h","f","f","h","f","f","f","h","h","f"))
#tester la fonction
print(test_mann_whitney(x,y))

#fonction native de R (approximation normale, pas de correction de continuité)
wilcox.test(x ~ y,exact=FALSE,correct=FALSE)
