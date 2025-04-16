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
        # On utilise is_valid_so_far pou la validité locale de la cellule
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

