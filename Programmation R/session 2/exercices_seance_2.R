# ================================================== Helper functions
# Fonction factorielle
factorial <- function(n) {
  fact <- 1
  for (i in 1:n) {
    fact <- fact * i
  }
  return(fact)
}


# Calcul l'aire sous la courbe de fonction suivant la loi de probabilité voulu.
trapeze <- function(densite, a, b, epsilon, ...) {
  
  # limite gauche du trapeze.
  increment <- a
  
  # vecteur pour stocker les aires.
  aire <- 0
  
  
  while ((increment + epsilon) < b) {
    aire <- aire + (epsilon * ((densite(increment, ...) + densite(increment + epsilon, ...))/2))
    
    # Augmenter la variable d'incrémentation de +epsilon.
    increment <- increment + epsilon
  }
  
  # Rajouter le dernier espilon réduit si depassement de borne.
  if (increment + epsilon > b) {
    new_eps <- b - increment
    aire <- aire + (epsilon * ((densite(increment, ...) + densite(increment + new_eps, ...))/2))
  }
  
  return(aire)
}


# Méthode de Simpson
simpson <- function(densite, a, b, ...) {
  
  fa <- densite(a, ...)
  fm <- densite((a + b)/2, ...)
  fb <- densite(b, ...)
  
  membre1 <- (b-a)/6
  membre2 <- fa + (4 * fm) + fb
  return(membre1 * membre2)
}



# Calcul de la valeur de la loi normale
norm <- function(x, ...) {
  return((1/(sqrt(2*pi))) * exp(-(x**2)/ 2))
}



# Densite de la loi du KHI2
khideux <- function(x, ddl, ...) {
  numerateur <- x**((ddl/2) - 1) * exp(-x/2)
  denominateur <- 2**(ddl/2) * gamma(ddl/2)
  return(numerateur/denominateur)
}

#======================================= Exercices


# # ------------------------------------1. PGCD - Plus grand commun diviseur

pgcd <- function(a, b) {
  
  while (a != b) {
    
    ecart <- abs(a - b)
    
    if (a > b) {
      a <- ecart
    } else {
      b <- ecart
    }
  }
  
  return(a)
}


# ------------------------------------2. Calcul amplitude d’intervalles – Intervalles de largeurs égales
intervalle <- function(a, b, k=3) {
  
  if (a > b | k == 0) {
    return(NA)
  }
  
  return (b-a)/k

}


# ------------------------------------3. Simulation
simulation <- function() {
  
  result <- 100000
  
  # nombre d'année
  year <- 3
  
  for (i in 1:year) {
    prob <- runif(1)
    if (prob < 0.2) {
      result <- result + 10000
    } else if (prob < 0.7) {
      result <- result
    } else {
      result <- result - 5000
    }
  }
  
  return(result)
}


allsimulation <- function(n = 1000) {
  
  # nombre d'année
  year <- 3
  
  allresults <- c()
  
  
  for (i in 1:n) {
    
    result <- 100000
    
    for (i in 1:year) {
      prob <- runif(1)
      if (prob < 0.2) {
        result <- result + 10000
      } else if (prob < 0.7) {
        result <- result
      } else {
        result <- result - 5000
      }
    }
    
    allresults <- c(allresults, result)
  }

  return(mean(allresults))
}

# ------------------------------------4. Fonction de Répartition de la loi de Poisson
fpoisson <- function(x, lambda) {

  result <- c()
  
  for (i in 1:x) {
    
    value <- exp(-lambda) * ((lambda**i) / factorial(i))
    
    result <- c(result, value)
  }
  
  return(sum(result))
}


# ------------------------------------5. Aire entre deux bornes de la fonction de densité de la loi normale
loi_normale <- function(a, b, epsilon=0.001, surface=trapeze) {
  return(surface(norm, a, b, epsilon))
}

# ------------------------------------6. Fonction de répartition de la loi normale
loi_normale_plus <- function(b, epsilon=0.001, surface=trapeze) {
  
  if (b > 0) {
    return(0.5 + loi_normale(0, b, epsilon=epsilon, surface=surface))
  } else if (b < 0) {
    return((1 - (loi_normale(b, -b, epsilon=epsilon, surface=surface))) / 2)
  } else {
    return(0)
  }
}

# ------------------------------------7. Fonction de répartition de la loi du KHI-2
# Calcul de la surface d'un trapèze
loi_khideux <- function(b, ddl, epsilon=0.001, surface=trapeze) {
  if (b < 0) {
    return(NA)
  }
  
    aire <- surface(khideux, 0, b, epsilon = epsilon, ddl = ddl)
    
    return(aire)
  }
# ------------------------------------8. Méthode de Simpson (redefinition des fonctions de calcul de surface 
# en permettant l'utilisant de la méthode de Simpson.)



# ------------------------------------9. Suite de Fibonacci
fibonacci <- function(n) {
  
  # Verification nombre entier
  if (n %% 1 != 0) {
    stop("Nombre non entier!")
  }
  
  if (n == 1) {
    return(1)
  }  else if (n == 2) {
    return(1)
  }
  
  return(fibonacci(n-1) + fibonacci(n-2))
}

# ------------------------------------10) Résolution d’équation – Méthode de dichotomie
dichotomie <- function(FONCTION, a, b, epsilon=0.0001) {
  
  # Si les fonctions sont du même signe, retourner NA.
  if ((a >b) | (FONCTION(a) * FONCTION(b) > 0)) {
    return(NA)
  }
  
  while (b - a > epsilon) {
    m <- (a + b) / 2
    
    # si f(m) n'est pas du même signe de f(a) alors m devient la limite droite.
    if (FONCTION(a) * FONCTION(m) <= 0) {
      b <- m
    } else {
      
      # si f(m) est du même signe que a, alors le 0 se situ entre m et b (m devient la limite gauche)
      a <- m
    }
  }
  
  return(m)
}

# ------------------------------------ 11) Seuils des bâtons brisés en ACP
batons_brises <- function(p) {
  
  seuils <- c()
  
  # Calcul pour chaque composante.
  for (k in 1:p) {
    
    seuil <- 0
    
    # Calcul des batons brisés pour la composante concerné.
    for (i in k:p) {
      seuil <- seuil + (1/i)
    }
    
    seuils <- c(seuils, seuil)
  }
  
  names(seuils) <- (1:p)
  
  return(seuils)
}

# ------------------------------------ 12. Probabilité simulée (méthode de Monte Carlo)

proba_simulee <- function(n, seuil) {
  
  superieur <- 0
  inferieur <- 0
  
  for (i in 1:n) {
    value <- rnorm(1)
    
    ifelse(value <= seuil, inferieur <- inferieur + 1, superieur <- superieur + 1)
  }
  
  return(superieur/(inferieur + superieur))
  
}

# ------------------------------------ 13. Anagrammes.
combinatoire <- function(chaine) {
  
  # Vecteurs de resultats.
  anagrammes <- c()
  
  recursion <- function(current, source) {
    
    # Si il n'ya plus de lettre, ajouter l'anagramme au vecteur de résultats.
    if (nchar(source) == 1) {
      anagrammes <<- c(anagrammes, paste0(current, source))
      return(NULL)
    }
    
    for (i in 1:nchar(source)) {
      
      # Rajouter la lettre à la chaine reponse.
      nextcurrent <- paste0(current, substr(source, i, i))
      
      # Supprimer la lettre récupérée de la source.
      nextsource <- paste0(substr(source, 1, i-1), substr(source, i+1, nchar(source)))
      
      # lancer l'appel récursif sur les lettres restantes.
      recursion(nextcurrent, nextsource)
    }
  }
  
  # Lancer la recursion et imprimmer le vecteur d'anagramme.
  recursion("", chaine)
  
  return(anagrammes)
}

# ------------------------------------ 14. Optimisation.
optim_production <- function(n1, n2, n3) {
  
  # Vecteur de poutres
  poutres <- c(3)
  
  # Nombre poutres.
  n <- 1
  
  
  # Fonction de placement de troncons.
  place <- function(longueur, k) {
    
    if (k == 0) {
      return()
    }
    
    # placer les n3 (commencer par les troncons les plus grands)
    for (i in 1:k){
      
      # Variable pour trouver une place à la poutre.
      placed <- FALSE
      
      # Iteration sur les poutres disponibles
      for (j in 1:n) {
        if (longueur <= poutres[j]) {  
          poutres[j] <<- poutres[j] - longueur
          placed <- TRUE
          break
        }
      } # Endfor
      
      # Si le troncon n'a pas pu être placé, creer une nouvelle poutre
      if (!placed) {
        poutres <<- c(poutres, 3 - longueur)
        n <<- n + 1
      }
      
    } # endfor
    
  } # End function
  
  # Application de la fonction pour chacun des troncons en commencant par les plus gros.
  place(2, n3)
  place(1.5, n2)
  place(0.5, n1)
  
  # Retourner le nombre de poutres et imprimer le reste de chute.
  print(paste("Longueur des chutes: ", sum(poutres)))
  return(n)
}


