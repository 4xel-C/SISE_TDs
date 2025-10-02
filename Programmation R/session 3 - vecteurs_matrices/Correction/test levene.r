#librairies pour anova
source("anova.r")

#ma fonction
my_levene <- function(x,y){
  if (is.factor(y) == FALSE || nlevels(y) < 2){
    return (NA)
  } else
  {
    #centrer les données intra-groupes
    #sur leurs moyennes respectives
    dc <- unlist(tapply(x,y,function(x){abs(x-mean(x))}))
    #anova sur données transformées
    return(test_anova_tapply(dc,y))
  }
}

#données iris
data(iris)

#essai
my_levene(iris$Petal.Width,iris$Species)


#vérif. package "car"
library(car)

#test de Levene
car::leveneTest(iris$Petal.Width,iris$Species,center="mean")

