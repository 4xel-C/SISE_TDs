comp_moyenne <- function(v1,v2){
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

test_student <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) != 2 || length(x) != length(y)){
    return (NULL)
  } else
  {
    #pour le 1er groupe
	#ou bien v1 <- x[unclass(y)==1]
    v1 <- x[y==levels(y)[1]]
    #pour le second groupe
    v2 <- x[y==levels(y)[2]]
    #appel de comp.moyenne - Exercice 1
    return(comp_moyenne(v1,v2))
  }
}

#données pour tester la fonction
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor(c("h","f","f","h","f","f","f","h","h","f"))

#tester la fonction
print(test_student(x,y))

