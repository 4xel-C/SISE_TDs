#sans les boucles grâce à tapply
kwallis <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) < 2){
    return (NA)
  } else
  {
    n <- length(x)
    K <- nlevels(y)
    effectifs <- tapply(x,y,length) #effectifs conditionnels
    rangs <- rank(x) #rangs globaux
    m_rangs_k <- tapply(rangs,y,mean) #moyenne des rangs conditionnels
    #stat de test
    KW <- 12/(n*(n+1))*sum(effectifs*(m_rangs_k-(n+1)/2)^2)
    #ddl
    ddl <- K-1
    #p-value
    p <- pchisq(KW,ddl,lower.tail=FALSE)
    #return
    return(list(K=KW,df=ddl,p.value=p))
  }
}

#données pour tester la fonction
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor(c("h","f","f","h","f","f","f","h","h","f"))

#tester la fonction
print(kwallis(x,y))

#fonction de R
print(kruskal.test(x ~ y))
