#fonction pour comparer les moyennes
comp_moyenne <- function(v1, v2){
  #calcul des moyennes
  m1 <- mean(v1)
  m2 <- mean(v2)
  #effectifs
  n1 <- length(v1)
  n2 <- length(v2)
  #écarts-type
  s1 <- sd(v1)
  s2 <- sd(v2)
  #calcul de s^2
  scarre <- ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)
  #t de student
  t.stud <- (m1-m2)/(sqrt(scarre)*sqrt(1/n1+1/n2))
  #p-value -- attention, le test est bilatéral !!!
  p.stud <- 2*pt(abs(t.stud),df=n1+n2-2,lower.tail=FALSE)
  #renvoyer le tout
  return(list(t=t.stud,ddl=n1+n2-2,pvalue=p.stud)) 
}

#deux vecteurs 
v1 <- c(159,170,190,167,160,155)
v2 <- c(185,168,179,176)
#appel de votre fonction programmée ci-dessus
z <- comp_moyenne(v1,v2)
print(z)
#pour vérification - le test fait par R
print(t.test(v1,v2,var.equal=T))

