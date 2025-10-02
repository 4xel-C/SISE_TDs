#(avec boucles) en passant par la SCR
test_anova_by_scr <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) < 2){
    return (NA)
  } else
  {
    n <- length(x)
    K <- nlevels(y)
    #SCT
    SCT <- (n-1)*var(x)
    #SCR
    SCR <- 0
    for (k in 1:K){
      xk <- x[unclass(y)==k]
      nk <- length(xk)
      SCR <- SCR + (nk-1)*var(xk)
    }
    #SCE
    SCE <- SCT - SCR
    #print(SCE)
    #print(SCR)
    #F de l'Anova
    Fa <- (SCE/(K-1))/(SCR/(n-K))
    #ddl1
    ddl1 <- K-1
    ddl2 <- n-K
    #p-value
    p <- pf(Fa,ddl1,ddl2,lower.tail=FALSE)
    #return
    return(list(F.Anova=Fa,ddl1=ddl1,ddl2=ddl2,p.value=p))
  }
}

#(avec boucles) en passant par la SCE
test_anova_by_sce <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) < 2){
    return (NA)
  } else
  {
    n <- length(x)
    K <- nlevels(y)
    #SCT
    SCT <- (n-1)*var(x)
    MGLOB <- mean(x)
    #SCE
    SCE <- 0
    for (k in 1:K){
      xk <- x[unclass(y)==k]
      nk <- length(xk)
      SCE <- SCE + nk*(mean(xk)-MGLOB)^2
    }
    #SCR
    SCR <- SCT - SCE
    #F de l'Anova
    Fa <- (SCE/(K-1))/(SCR/(n-K))
    #ddl1
    ddl1 <- K-1
    ddl2 <- n-K
    #p-value
    p <- pf(Fa,ddl1,ddl2,lower.tail=FALSE)
    #return
    return(list(F.Anova=Fa,ddl1=ddl1,ddl2=ddl2,p.value=p))
  }
}

#sans les boucles grâce à tapply
test_anova_tapply <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) < 2){
    return (NA)
  } else
  {
    n <- length(x)
    K <- nlevels(y)
    #SCT
    SCT <- (n-1)*var(x) #ou encore SCT <- sum((x-mean(x))^2)
    MGLOB <- mean(x)
    #SCE
    moyennes <- tapply(x,y,mean) #moyennes conditionnelles
    effectifs <- tapply(x,y,length) #effectifs conditionnels
    SCE <- sum(effectifs*(moyennes-MGLOB)^2)
    #SCR
    SCR <- SCT - SCE
    #F de l'Anova
    Fa <- (SCE/(K-1))/(SCR/(n-K))
    #ddl
    ddl1 <- K-1
    ddl2 <- n-K
    #p-value
    p <- pf(Fa,ddl1,ddl2,lower.tail=FALSE)
    #return
    return(list(F.Anova=Fa,ddl1=ddl1,ddl2=ddl2,p.value=p))
  }
}

#donn?es pour tester la fonction
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor(c("h","f","f","h","f","f","f","h","h","f"))
#tester la fonction
print(test_anova_by_scr(x,y))
#tester la fonction
print(test_anova_by_sce(x,y))
#tester la fonction
print(test_anova_tapply(x,y))

#avec la fonction aov de R
summary(aov(x ~ y))

#autre essai
data(iris)
print(test_anova_tapply(iris$Petal.Width,iris$Species))

#et aov()
summary(aov(iris$Petal.Width ~ iris$Species))
