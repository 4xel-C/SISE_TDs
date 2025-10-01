# Vecteurs de test.
V1 <- c(159,170,190,167,160,155)
V2 <- c(185,168,179,176)


# ------------------------ Exercice 1
comp.moyenne <- function(v1, v2) {
  
  # Calcul des moyennes
  m1 <- mean(v1)
  m2 <- mean(v2)
  
  # Calcul des variances
  s1 <- sd(v1) **2
  s2 <- sd(v2) **2
  
  # Calcul du nombre d'éléments
  n1 <- length(v1)
  n2 <- length(v2)
  
  # Calcul de la variance totale.
  var <- ((n1-1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2)
  
  # Calcul du t de student.
  t <- (m1 - m2) / (sqrt(var) * sqrt((1/n1) + (1/n2)))
  
  # Calcul de la pvalue.
  p.value <- 2 * pt(abs(t), (n1 + n2 - 2), lower.tail = FALSE)
  
  ddl <- n1 + n2 - 2
  
  # Renvoyer une liste
  return(list(t = t, p.value = p.value, ddl = ddl))
}



# ------------------------ Exercice 2
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor (c("h", "f", "f", "h", "f", "f", "f", "h", "h", "f"))


test_student <- function(x, y) {
  if (!is.factor(y) | !length(unique(y))) {
    stop("y n'est pas un factor ou ne contient pas le bon n ombre de facteurs")
  }
  
  # Calcul du vecteur de moyennes.
  means <- tapply(x, y, mean)
  
  # Calcul du vecteur d'ecart type
  std <- tapply(x, y, sd)
  
  # Calcul des variances.
  s1 <- std[1]**2
  s2 <- std[2]**2
  
  # Calcul des moyennes
  m1 <- means[1]
  m2 <- means[2]
  
  # Calcul des frequences absolues
  freq <- tapply(x, y, length)
  n1 <- freq[1]
  n2 <-  freq[2]
  
  # Calcul de la variance totale.
  var <- ((n1-1) * s1 + (n2 - 1) * s2) / (n1 + n2 - 2)
  
  # Calcul du t de student.
  t <- (m1 - m2) / (sqrt(var) * sqrt((1/n1) + (1/n2)))
  
  # Calcul de la pvalue.
  p.value <- 2 * pt(abs(t), (n1 + n2 - 2), lower.tail = FALSE)
  
  ddl <- n1 + n2 - 2
  
  # Renvoyer une liste
  return(list(t = t, p.value = p.value, ddl = ddl))
}


test_student_vectors_manipulations <- function(x, y) {
  
  if (!is.factor(y) | !length(unique(y))) {
    stop("y n'est pas un factor ou ne contient pas le bon nombre de facteurs")
  }
  
  # Scinder les valeurs en deux groupes.
  x_hommes <- x[y == "h"]
  x_femmes <- x[y == "f"]
  
  return(comp.moyenne(x_hommes, x_femmes))
}


# ------------------------ Exercice 3
comp_moyenne_pairwise <- function(v1, v2) {
  if(length(v1) != length(v2)) {
    return(NULL)
  }
  
  # Calcul de n
  n <- length(v1)
  
  # Calcul du vecteur di
  di <-  v1 - v2
  
  # Calcul du d moyen
  dmoy <-  mean(di)
  
  # Calcul du sd.carre
  sd.carre <-  (1/(n-1)) * sum((di - dmoy)**2)
  std <-  sqrt(sd.carre)
  
  # Calcul du t de student
  t <-  dmoy / (std/sqrt(n))
  
  ddl <- n - 1
  
  p.value <- 2 * pt(abs(t), ddl, lower.tail = FALSE)
  
  return(list(t = t, p.value = p.value, ddl = ddl))
}



# ------------------------ Exercice 4
freqmax <- function(x) {
  if (!is.factor(x)) {
    stop("Le vecteur entré n'est pas un factor.")
  }
  
  factmax <- names(which.max(table(y)))
  valmax <- max(table(y))
  
  return(list("modalité" = factmax, "valeur"=valmax))
}


# ------------------------ Exercice 5
x <- c(1,2,2, 3,2, 2, 1, 1, 2, 2, 1, 1, 1,1,2)
y <- c(1, 2,1, 2, 1, 2, 1, 1, 2, 1,2,1,1,2,2)
x <- factor(x)
y <- factor(y)

test.khi2 <- function(x, y) {
  
  # Gestion d'erreurs.
  if (!is.factor(y) | !is.factor(x) | length(x) != length(y)) {
    return(NULL)
  }
  
  # Génération du tableau de contingence.
  cont <- table(x, y)
  
  # Nombres d'éléments
  n <- sum(cont)
  
  # 
  CHI2 <- 0
  
  # Matrice des effectifs théoriques.
  for (row in 1:nrow(cont)) {
    for (col in 1:ncol(cont)) {
      e <- ((sum(cont[row, ]) * sum(cont[, col])) / n)
      nlc <-  cont[row, col]
      
      CHI2 <- sum(CHI2, ((nlc - e)**2)/e)
    }
  }
  
  ddl <- (nrow(cont) - 1) * (ncol(cont) - 1)
  
  p.value <- pchisq(CHI2, ddl, lower.tail = FALSE)
  
  return(list("chi2" = CHI2, ddl = ddl, p.value = p.value))
}


# ------------------------ Exercice 6

m <- matrix(c(33,94,21,63,452,115,4,13,5,8,154,38),nrow=3,ncol=4)

residus.ajustes <- function(m){
  
  # Calcul de n
  n <- sum(m)
  
  # Matrice result initialisation
  result <- matrix(0, nrow = nrow(m), ncol = ncol(m))
  
  rownames(result) <- rownames(m)
  colnames(result) <- colnames(m)
    
    
  # Matrice des effectifs théoriques.
  for (row in 1:nrow(m)) {
    for (col in 1:ncol(m)) {
      nl <- sum(m[row,])
      nc <- sum(m[, col])
      
      
      e <- (nl * nc) / n
      nlc <-  m[row, col]
      
      result[row, col] <- (nlc - e) / sqrt(e * (1 - nl/n) * (1 - nc / n))
    }
  }
  
  return(result)
}

# ------------------------ Exercice 7

profil.ligne <- function(x) {
  
  # Calcul des frequences marginales lignes.
  n.ligne <- apply(x, FUN = sum, MARGIN = 1)
  
  # Calcul du profil ligne.
  result <- sweep(x, n.ligne, FUN = "/", MARGIN = 1)
  
  return(result)
}

profil.colonne <- function(x) {
  
  # Calcul des frequences marginales colonnes
  n.colonne <- apply(x, FUN = sum, MARGIN = 2)
  
  # Calcul du profil ligne.
  result <- sweep(x, n.colonne, FUN = "/", MARGIN = 2)
  
  return(result)
}

# ------------------------ Exercice 8
x <- c(185,159,170,168,190,167,160,179,176,155)
y <- as.factor(c("h","f","f","h","f","f","f","h","h","f"))

test_anova_tapply <- function(x, y){
  if (!is.factor(y) | (length(unique(y))) < 2) {
    return(NULL)
  }
  
  # Calcul de la moyenne.
  meantot <- mean(x)
  
  # Calcul du vecteur de moyenne conditionnelles
  meank <- tapply(x, y, mean)
  
  # Calcul du vecteur de fréquence.
  nk <- tapply(x, y, length)
  
  # Calcul du nombre de fréquences.
  n <- length(x)
  
  # nombre de groupes
  K <- length(levels(y))
  
  SCE <- 0
  SCR <- 0
  SCT <- 0
  
  # Calcul du SCE
  SCE <- sum(nk * (meank - meantot)**2)
  
  # Calul du SCT
  SCT <- sum((x - meantot)**2)
  
  # Calcul du SCR
  SCR = SCT - SCE
  
  # Calcul du F
  numerateur <- SCE / (K - 1)
  denom <- SCR / (n - K)
  
  f <- numerateur / denom
  
  # Calcul de la pvalue
  pvalue <- pf(f, K-1, n-K, lower.tail=FALSE)
  
  return(list("F"=f, ddl1 = K-1, ddl2 = n - K, pvalue = pvalue))
}

# ------------------------ Exercice 9
A <- c(185,168,179,176)
B <- c(159,170,190,167,160,155)


mann_whitney <- function(a, b) {
  
  # Calcul des fréquences absolues.
  n1 <- length(A)
  n2 <- length(B)
  n <- n1 + n2
  
  # Calcul des sommes des rangs
  names(A) <- rep("A", times = length(A))
  names(B) <- rep("B", times = length(B))
  
  rank_global <- rank(c(A, B))
  
  S1 <- sum(rank_global[names(rank_global) == "A"])
  S2 <- sum(rank_global[names(rank_global) == "B"])
  
  # Calcul des statistiques U1 et U2
  U1 <- S1 - (n1*(n1 + 1))/2
  U2 <- S2 - (n2*(n2 + 1))/2
  
  U <- min(U1, U2)
  
  
}

