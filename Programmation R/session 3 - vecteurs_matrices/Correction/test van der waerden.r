#test de van der waerden
test_vdw <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) < 2 || length(x) != length(y)){
    return (NA)
  } else
  {
    #effectif
    n <- length(x)
    #calculer les scores
    rangs <- rank(x,ties.method='average')
    scores <- qnorm(rangs/(n+1))
    #effectifs par groupe
    n_k <- tapply(scores,y,length)
    #moyenne globale des scores
    scores_barre <- mean(scores)
    #variance globale des scores
    T2 <- var(scores)
    #somme des scores conditionnels
    S_k <- tapply(scores,y,function(x){sum(x)})
    #numérateur de la stat de test
    num <- sum(sapply(1:nlevels(y),function(k){1/n_k[k]*(S_k[k]-n_k[k]*scores_barre)^2}))
    #stat de test
    C <- num/T2
    #ddl
    ddl <- nlevels(y)-1
    #p-value
    pvalue <- pchisq(C,ddl,lower.tail = FALSE)
    #résultat
    return(list(statistic=C,ddl=ddl,p_value=pvalue))
  }
}

#tester
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor(c("h","f","f","h","f","f","f","h","h","f"))

#tester la fonction
print(test_vdw(x,y))

#avec DescTools
library(DescTools)
print(DescTools::VanWaerdenTest(x,y))
