#' Backtracking pour avoir une grille complète juste
#'
#' #' Remplir récursivement la grille avec Backtracking
#'
#' @param board Matrice en cours de remplissage.
#' @param row Numéro de la ligne en cours.
#' @param col Numéro de la colonne en cours.
#' @return La matrice complétée si une solution est trouvée, ou \code{NULL} sinon.
#' @keywords internal
fill_board <- function(X, ligne, col) {
  n <- nrow(X)
  if (ligne > n) {
    return(X)
  }
    next_col <- col + 1
  next_ligne <- ligne
  if (next_col > n) {
    next_col <- 1
    next_ligne <- ligne + 1
  }
  
  for (val in c(0, 1)) {
    X[ligne, col] <- val
    
    if (is_valid_so_far(X, ligne, col)) {
      result <- fill_board(X, next_ligne, next_col)
      if (!is.null(result)) {
        return(result)
      }
    }
    X[ligne, col] <- NA
  }
    return(NULL)
}




#' Génère une solution Takuzu complète
#'
#' Tout est dans le titre
#' 
#'
#' @param n  taille de la grille (doit être pair (ex. 6, 8).)
#' @return Une matrice \code{n x n} avec de 0 et 1.
generate_takuzu_solution <- function(n) {
  X <- matrix(NA, nrow = n, ncol = n)
  res <- fill_board(X, 1, 1)
  return(res)
}






#' Génère un puzzle Takuzu
#'
#' On génère une grille complète et on enleve des cases pour jouer
#'
#' @param n taille
#' @param difficulty proportion enlevé
#' @return le plateau de jeu
#' @export
generate_takuzu <- function(n = 6, difficulty = 0.5) {
  if (n %% 2 != 0) {
    stop("La taille n doit être un nombre pair.")
  }
  if (difficulty < 0 || difficulty > 1) {
    stop("Le paramètre 'difficulty' doit être compris entre 0 et 1.")
  }
  solution <- generate_takuzu_solution(n)
  
  puzzle <- solution
  cases <- n * n
  nb_to_remove <- floor(cases * difficulty)
  
  delete <- sample(cases, nb_to_remove)
  puzzle[delete] <- NA
  
  return(puzzle)
}







# Les fonction utilisé pour les fonctions du package



#' Vérifie la validité partielle d'une case dans une grille Takuzu
#'
#' Vérifie si la valeur placée dans la cellule (row, col) ne viole pas les règles du Takuzu jusqu'à présent.
#'
#' @param board Matrice partiellement remplie.
#' @param row Ligne de la case à vérifier.
#' @param col Colonne de la case à vérifier.
#' @return \code{TRUE} si c'est valide, \code{FALSE} sinon.
#' @keywords internal
is_valid_so_far <- function(board, row, col) {
  val <- board[row, col]
  if (is.na(val)) return(TRUE)
  
  n <- nrow(board)
  
  row_values <- board[row, ]
  
  if (has_three_consecutive(row_values)) {
    return(FALSE)
  }
  if (!any(is.na(row_values))) {
    if (sum(row_values == 0) != sum(row_values == 1)) {
      return(FALSE)
    }
  }
  
  col_values <- board[, col]
  
  if (has_three_consecutive(col_values)) {
    return(FALSE)
  }
  if (!any(is.na(col_values))) {
    if (sum(col_values == 0) != sum(col_values == 1)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}


#' Détecte la présence de 3 valeurs identiques consécutives dans un vecteur
#'
#' @param x Un vecteur (ligne ou colonne).
#' @return \code{TRUE} s'il y a 3 valeurs identiques consécutives, \code{FALSE} sinon.
#' @keywords internal
has_three_consecutive <- function(x) {
  for (i in seq_len(length(x) - 2)) {
    if(all(!is.na(x[i:(i+2)]))) {
      if(x[i] == x[i+1] && x[i+1] == x[i+2]) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}
