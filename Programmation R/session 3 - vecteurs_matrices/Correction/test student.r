test_student <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) != 2 || length(x) != length(y)){
    return (NA)
  } else
  {
    #pour le 1er groupe
    g1 <- (y==levels(y)[1]) #vecteur de booleen indiquant les individus du 1er groupe
    n1 = length(x[g1])
    xb1 = mean(x[g1])
    v1 = var(x[g1])
    #pour le second groupe
    g2 <- (y==levels(y)[2])
    n2 = length(x[g2])
    xb2 = mean(x[g2])
    v2 = var(x[g2])
    #construction de la statistique de test
    ddl = n1 + n2 - 2
    v = (n1-1)*v1 + (n2-1)*v2
    d = xb1-xb2
    t = d/sqrt((v/ddl)*(1/n1+1/n2))
    p = 2*pt(abs(t),ddl,lower.tail=FALSE)
    #renvoyer le tout
    return(list(t=t,ddl=ddl,pvalue=p))
  }
}

#donnees pour tester la fonction
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor(c("h","f","f","h","f","f","f","h","h","f"))
#tester la fonction
print(test_student(x,y))
