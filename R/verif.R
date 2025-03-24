#' Vérifie qu'une grille Takuzu est valide
#'
#' Cette fonction s'assure qu'une grille complétée (sans \code{NA}) respecte
#' les règles du Takuzu : pas plus de 2 zéros/uns consécutifs, et autant de 0 que de 1
#' dans chaque ligne et colonne.
#'
#' @param board Une matrice \code{n x n} de 0 et 1 (pas de \code{NA}).
#' @return \code{TRUE} si la grille est valide, \code{FALSE} sinon.
#' @export
check_takuzu <- function(board) {
  if (any(is.na(board))) {
    warning("La grille contient des NA, vérification incomplète.")
    return(FALSE)
  }

  n <- nrow(board)

  # Vérification de chaque ligne
  for (r in 1:n) {
    row_vals <- board[r, ]
    # 1) Pas de séquence de 3 identiques
    if (has_three_consecutive(row_vals)) {
      return(FALSE)
    }
    # 2) Autant de 0 que de 1
    if (sum(row_vals == 0) != sum(row_vals == 1)) {
      return(FALSE)
    }
  }

  # Vérification de chaque colonne
  for (c in 1:n) {
    col_vals <- board[, c]
    if (has_three_consecutive(col_vals)) {
      return(FALSE)
    }
    if (sum(col_vals == 0) != sum(col_vals == 1)) {
      return(FALSE)
    }
  }

  return(TRUE)
}


#' Vérifie si une grille Takuzu est entièrement résolue
#'
#' Une grille est dite "résolue" si elle ne contient aucun \code{NA} et qu'elle
#' satisfait toutes les règles du Takuzu (voir \code{\link{check_takuzu}}).
#'
#' @param board Une matrice \code{n x n} de 0, 1 et éventuellement \code{NA}.
#' @return \code{TRUE} si la grille est entièrement résolue, \code{FALSE} sinon.
#' @export
is_solved_takuzu <- function(board) {
  # D'abord vérifier qu'il n'y a plus de cases vides
  if (any(is.na(board))) {
    return(FALSE)
  }
  # Ensuite vérifier les règles du Takuzu
  return(check_takuzu(board))
}

# Fonction pour valider les règles du Takuzu
is_valid_takuzu <- function(grid) {
  n <- nrow(grid)

  # Vérifier les lignes
  for (i in 1:n) {
    # Vérifier qu'il y a un nombre égal de 0 et 1 dans chaque ligne
    if (sum(grid[i, ] == 0, na.rm = TRUE) != sum(grid[i, ] == 1, na.rm = TRUE)) {
      return(FALSE)
    }
    # Vérifier qu'il n'y a pas de deux chiffres consécutifs identiques
    if (any(diff(grid[i, ], na.rm = TRUE) == 0)) {
      return(FALSE)
    }
  }

  # Vérifier les colonnes
  for (j in 1:n) {
    # Vérifier qu'il y a un nombre égal de 0 et 1 dans chaque colonne
    if (sum(grid[, j] == 0, na.rm = TRUE) != sum(grid[, j] == 1, na.rm = TRUE)) {
      return(FALSE)
    }
    # Vérifier qu'il n'y a pas de deux chiffres consécutifs identiques
    if (any(diff(grid[, j], na.rm = TRUE) == 0)) {
      return(FALSE)
    }
  }

  return(TRUE)
}

