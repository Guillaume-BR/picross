# Fonction pour vérifier les positions des "1" dans les matrices
#' Vérifie la position de 1 dans deux matrices
#' 
#' @param matrice1 : première matrice
#' @param matrice2 : deuxième matrice
#' @returns TRUE or FALSE
#' @examples
#' verifier_positions(matrix(c(1,0,1,1), nrow = 2 , ncol=2),matrix(c(1,0,1,1), nrow = 2 , ncol=2))
#' verifier_positions(matrix(c(1,0,1,1,1,0), nrow = 2 , ncol=3),matrix(c(1,0,1,0,1,0), nrow = 2 , ncol=3))
verifier_positions <- function(matrice1, matrice2) {
  positions_matrice1 <- which(matrice1 == 1, arr.ind = TRUE)
  positions_matrice2 <- which(matrice2 == 1, arr.ind = TRUE)
  
  return(identical(positions_matrice1, positions_matrice2))
}
