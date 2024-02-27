# Définir le dictionnaire des niveaux de difficulté
niveaux_difficulte <- list(
  cadeau = 0.8,
  facile = 0.7,
  moyen = 0.6,
  difficile = 0.55,
  impossible = 0.4
)

tailles <- list(
  "5x5" = 5,
  "10x10" = 10,
  "15x15" = 15
)
  

# Fonction pour générer une matrice aléatoire en fonction du niveau de difficulté
mat_alea <- function(taille, niveau_difficulte) {
  # Vérifier si le niveau de difficulté est défini dans le dictionnaire
  if (!exists(niveau_difficulte, where = niveaux_difficulte)) {
    stop("Niveau de difficulté non valide.")
  }
  
  #vérifier si la taille est présente
  if (!exists(taille, where = tailles)) {
    stop("Taille non valide.")
  }
  
  # Obtenir la probabilité correspondante
  proba_1 <- niveaux_difficulte[[niveau_difficulte]]
  
  #Obtenir le taille correspondate
  taille_1 <- tailles[[taille]]
  
  # Générer une matrice avec des 1 et des 0 en fonction des probabilités
  matrice_aleatoire <- matrix(sample(c(0, 1), taille_1 * taille_1, replace = TRUE, prob = c(1 - proba_1, proba_1)), nrow = taille_1, ncol = taille_1)
  
  return(matrice_aleatoire)
}

# Utiliser la fonction pour générer une matrice aléatoire
resultat <- mat_alea("5x5", "moyen")

# Afficher la matrice résultante
print(resultat)

#fonction pour compter le nombre de 1 conscutifs par colonne (rle : run-length encoding)
nombre_1_consecutifs <- function(colonne) {
  rle_result <- rle(colonne)
  indices_1 <- which(rle_result$values == 1)
  longueurs_1 <- rle_result$lengths[indices_1]
  return(longueurs_1)
}

# Appliquer la fonction à chaque colonne
resultat_col <- apply(resultat, 2, nombre_1_consecutifs)

#trasnposition pour avoir les infos des lignes
resultat_row <- apply(t(resultat),2,nombre_1_consecutifs)

# Afficher les résultats
print(resultat_col)
print(resultat_row)

