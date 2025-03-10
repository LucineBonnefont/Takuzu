#' Génère une grille Takuzu incomplète
#'
#' Cette fonction génère une grille Takuzu de taille \code{n x n} (avec \code{n} pair),
#' en respectant les règles de base : pas plus de 2 zéros ou 2 uns consécutifs,
#' autant de 0 que de 1 par ligne/colonne dans la solution.
#'
#' @param n Taille de la grille (nombre de lignes et de colonnes), doit être pair (ex. 6, 8).
#' @param difficulty Un nombre entre 0 et 1 indiquant la proportion de cases à vider
#' (ex. 0.5 = on enlève 50% des cases).
#' @return Une matrice de taille \code{n x n} contenant des 0, 1 et \code{NA} (cases vides).
#' @examples
#' puzzle <- generate_takuzu(6, 0.5)
#' @export
generate_takuzu <- function(n = 6, difficulty = 0.5) {
  if (n %% 2 != 0) {
    stop("La taille n doit être un nombre pair.")
  }
  if (difficulty < 0 || difficulty > 1) {
    stop("Le paramètre 'difficulty' doit être compris entre 0 et 1.")
  }

  # Génère une solution complète
  solution <- generate_takuzu_solution(n)

  # Convertit cette solution en puzzle en retirant un pourcentage de cases
  puzzle <- solution
  total_cells <- n * n
  nb_to_remove <- floor(total_cells * difficulty)

  # On choisit au hasard quelles cases seront vides (NA)
  remove_indices <- sample(total_cells, nb_to_remove)
  puzzle[remove_indices] <- NA

  return(puzzle)
}


#' Génère une solution Takuzu complète
#'
#' Cette fonction interne construit une grille entièrement résolue (pas de NA),
#' respectant les règles du Takuzu. Elle utilise une approche backtracking pour
#' remplir la grille case par case.
#'
#' @param n Taille de la grille (pair).
#' @return Une matrice \code{n x n} remplie de 0 et 1.
#' @keywords internal
generate_takuzu_solution <- function(n) {
  board <- matrix(NA, nrow = n, ncol = n)
  res <- fill_board(board, 1, 1)
  return(res)
}


#' Fonction récursive de backtracking
#'
#' @param board Matrice en cours de remplissage.
#' @param row Numéro de la ligne en cours.
#' @param col Numéro de la colonne en cours.
#' @return La matrice complétée si une solution est trouvée, ou \code{NULL} sinon.
#' @keywords internal
fill_board <- function(board, row, col) {
  n <- nrow(board)

  # Si on a dépassé la dernière ligne, c'est que le board est rempli
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


#' Vérifie si la case (row, col) est valide jusqu'à présent
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
