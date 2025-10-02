#génération de la grille (7 x 7)
generer_grille <- function(){
  M <- matrix(rep(".",7*7),7,7)
  return(M)
}

#affichage de la grille
print_grille <- function(M){
 print(M)
}

#mise à jour de la grille
maj_grille <- function(M,i,j){
  M[i,j] <- "x"
  return(M)
}

#indication de direction de recherche
indication <- function(ib,jb,i,j){
  texte <- ""
  #vertical
  if (ib < i){texte <- "nord "}
  else if (ib > i){texte <- "sud "}
  #horizontal
  if (jb < j){texte <- paste(texte,"ouest")}
  else if (jb > j){texte <- paste(texte,"est")}
  #affichage
  print(paste("VOIR :",texte))
  #fin indication
}

#**** programme principal ****

#coord. ligne du bateau
bateau_lig <- sample(1:7,1)

#coord. col
bateau_col <- sample(1:7,1)

#trichons un peu
print(paste(bateau_lig,bateau_col))

#génération de grille initiale
grid <- generer_grille()

#affichage
print_grille(grid)

#lancement du jeu - 7 essais

#compteur d'essai
compteur = 0

#pas touché au début
touched = FALSE

#boucler
while (compteur < 7 && touched == FALSE){
  #incrémentation compteur
  compteur <- compteur + 1
  #coord. joueur
  print("Coord. lig = ")
  i <- scan()
  print("Coord. col = ")
  j <- scan()
  #vérification
  if (i == bateau_lig && j == bateau_col){
    touched = TRUE
  } else {
    print("--> EH NON, A L'EAU !")
    #màj de la grille
    grid <- maj_grille(grid,i,j)
    #affichage
    print_grille(grid)
    #indication
    indication(bateau_lig,bateau_col,i,j)
    print("==> PLAY AGAIN...")
  } #end if
}#end while

print("")
#alors ?
if (touched == TRUE){
  print("Bravo, nombre de coup(s)")
  print(compteur)
} else {
  print("Dommage, les bonnes coordonnées étaient ")
  print(paste(i,";",j))
}

#****  fin programme principal ****



