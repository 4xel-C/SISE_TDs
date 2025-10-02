comp_moyenne_pairwise <- function(v1, v2){
  if (length(v1) != length(v2)){
    return(NULL)
  } else {
    d <- v1-v2 #vecteur d'�carts
    m <- mean(d) #moyenne des �carts 
    e <- sd(d) #�cart-type des �carts
    n <- length(d) #effectif
    tstud <- m/(e / sqrt(n)) #stat. de test
    ddl <- n-1 #degr�s de libert�
    pvalue <- 2 * pt(abs(tstud),ddl,lower.tail=F) #p-value
    return(list(t=tstud,ddl=ddl,pvalue=pvalue))
  }
}
#deux vecteurs exemples
v1 <- c(1.2,3.4,5.6,7.8,1.3)
v2 <- c(3.6,1.7,5.6,9.0,1.5)
#appel de votre fonction programm�e ci-dessus
z <- comp_moyenne_pairwise(v1,v2)
print(z)
#pour v�rification - le test fait par R
print(t.test(v1,v2,paired=T))


