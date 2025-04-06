#' Backtracking pour avoir une grille complète juste
#'
#' #' Remplit récursivement la grille en utilisant un algorithme de backtracking pour obtenir une solution complète de Takuzu.
#'
#' @param board Matrice en cours de remplissage.
#' @param row Numéro de la ligne en cours.
#' @param col Numéro de la colonne en cours.
#' @return La matrice complétée si une solution est trouvée, ou \code{NULL} sinon.
#' @keywords internal
fill_board <- function(board, row, col) {
  n <- nrow(board)
  if (row > n) {
    return(board)
  }
  
  # Calcul de la case suivante
  next_col <- col + 1
  next_row <- row
  if (next_col > n) {
    next_col <- 1
    next_row <- row + 1
  }
  
  # On tente de placer 0 ou 1
  for (val in c(0, 1)) {
    board[row, col] <- val
    
    if (is_valid_so_far(board, row, col)) {
      # Appel récursif pour la case suivante
      result <- fill_board(board, next_row, next_col)
      if (!is.null(result)) {
        return(result)
      }
    }
    # Annule si pas valide
    board[row, col] <- NA
  }
  
  # Si aucune valeur ne fonctionne, on remonte (NULL)
  return(NULL)
}




#' Génère une solution Takuzu complète
#'
#' Cette fonction interne construit une grille entièrement résolue (pas de NA),
#' respectant les règles du Takuzu. Elle utilise une approche backtracking pour
#' remplir la grille case par case.
#'
#' @param n  taille de la grille (doit être pair (ex. 6, 8).)
#' @return Une matrice \code{n x n} avec de 0 et 1.
generate_takuzu_solution <- function(n) {
  board <- matrix(NA, nrow = n, ncol = n)
  res <- fill_board(board, 1, 1)
  return(res)
}






#' Génère un puzzle Takuzu
#'
#' On génère une grille complète et on enleve des cases 
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
  total_cells <- n * n
  nb_to_remove <- floor(total_cells * difficulty)
  
  remove_indices <- sample(total_cells, nb_to_remove)
  puzzle[remove_indices] <- NA
  
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
  
  # --- Vérification sur la ligne ---
  row_values <- board[row, ]
  
  # 1) Pas plus de 2 identiques consécutifs
  if (has_three_consecutive(row_values)) {
    return(FALSE)
  }
  # 2) Si la ligne est complète (pas de NA), elle doit contenir autant de 0 que de 1
  if (!any(is.na(row_values))) {
    if (sum(row_values == 0) != sum(row_values == 1)) {
      return(FALSE)
    }
  }
  
  # --- Vérification sur la colonne ---
  col_values <- board[, col]
  
  # 1) Pas plus de 2 identiques consécutifs
  if (has_three_consecutive(col_values)) {
    return(FALSE)
  }
  # 2) Si la colonne est complète (pas de NA), elle doit contenir autant de 0 que de 1
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
    # On ne teste que si les 3 valeurs sont non manquantes
    if(all(!is.na(x[i:(i+2)]))) {
      if(x[i] == x[i+1] && x[i+1] == x[i+2]) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}
