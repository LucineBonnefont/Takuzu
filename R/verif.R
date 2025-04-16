#' Test
#'
#'  fournir un " feedback " cellule par cellule sur la validité de la grille.
#' 
#'
#' @param board Une matrice \code{n x n} de 0, 1 et éventuellement \code{NA}.
#' @return Une matrice de dimension \code{n x n}.
#' @export
takuzu_feedback <- function(board) {
  n <- nrow(board)
  feedback <- matrix(TRUE, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (!is.na(board[i, j])) {
        feedback[i, j] <- is_valid_so_far(board, i, j)
      }
    }
  }
  return(feedback)
}




#' Is_valid_oui_ou_non
#' 
#' Est ce que la valeur rentré est juste ? cette fonction verifie ligne par ligne et colonne par colonne
#' 
#' @param board
#' @return  oui ou non
#' @export 
is_valid_takuzu <- function(grid) {
  n <- nrow(grid)
  
  #lignes
  for (i in 1:n) {
    if (sum(grid[i, ] == 0, na.rm = TRUE) != sum(grid[i, ] == 1, na.rm = TRUE)) {
      return(FALSE)
    }
    if (any(diff(grid[i, ], na.rm = TRUE) == 0)) {
      return(FALSE)
    }
  }
  
  #colonnes
  for (j in 1:n) {
    if (sum(grid[, j] == 0, na.rm = TRUE) != sum(grid[, j] == 1, na.rm = TRUE)) {
      return(FALSE)
    }
    if (any(diff(grid[, j], na.rm = TRUE) == 0)) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}


#' Vérifie qu'une grille Takuzu est valide
#'
#' Cette fonction s'assure qu'une grille complète (sans NA) respecte les règles du Takuzu
#'
#' @param board Une matrice n x n de 0 et 1.
#' @return TRUE si la grille est valide, FALSE sinon.
#' @export
check_takuzu <- function(board) {
  n <- nrow(board)
  for (r in seq_len(n)) {
    row_vals <- board[r, ]
    if (has_three_consecutive(row_vals)) return(FALSE)
    if (sum(row_vals == 0) != sum(row_vals == 1)) return(FALSE)
  }
  for (c in seq_len(n)) {
    col_vals <- board[, c]
    if (has_three_consecutive(col_vals)) return(FALSE)
    if (sum(col_vals == 0) != sum(col_vals == 1)) return(FALSE)
  }
  return(TRUE)
}


#' Vérifie si une grille Takuzu est entièrement résolue
#'
#' Verifie si y'a des NA qui reste
#'
#' @param board Une matrice \code{n x n} contenant des 0, des 1 et éventuellement des \code{NA} dans les cases non remplies.
#' @return \code{TRUE} si la grille est entièrement résolue, \code{FALSE} sinon.
#' @export
is_solved_takuzu <- function(board) {
  if (any(is.na(board))) {
    return(FALSE)
  }
  return(check_takuzu(board))
}

