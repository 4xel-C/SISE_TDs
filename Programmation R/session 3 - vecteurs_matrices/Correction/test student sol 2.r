test_student <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) != 2 || length(x) != length(y)){
    return (NA)
  } else
  {
    #calcul conditionnel
    n <- tapply(x,y,length)
    xb <- tapply(x,y,mean)
    vg <- tapply(x,y,var)
    #construction de la statistique de test
    ddl = n[1] + n[2] - 2
    v = (n[1]-1)*vg[1] + (n[2]-1)*vg[2]
    d = xb[1]-xb[2]
    t = d/sqrt((v/ddl)*(1/n[1]+1/n[2]))
    p = 2*pt(abs(t),ddl,lower.tail=FALSE)
    #renvoyer le tout
    return(list(t=t,ddl=ddl,pvalue=p))
  }
}

#données pour tester la fonction
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor(c("h","f","f","h","f","f","f","h","h","f"))
#tester la fonction
print(test_student(x,y))
