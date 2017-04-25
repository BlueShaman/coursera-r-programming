#' Creates a vector of the following four functions which can be used for effective manipulations with a
#' matrix (denoted by X) and it's inverse (denoted by X'):
#' 
#' 1. setMatrix(mat) - Sets the value of the matrix X. X must be invertible.
#' 2. getMatrix() - Returns the value of the matrix X.
#' 3. setInverse(matInverse) - Sets the value of the inverse matrix X'.
#' 4. getInverse() - Returns the value of the inverse matrix X'.
#' 
#' @param x Invertible matrix.
#' @return A list of the functions specified above.
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  setMatrix <- function(mat) {
    x <<- mat
    matrixInverse <<- NULL
  }
  getMatrix <- function() {
    x
  }
  setInverse <- function(matInverse) {
    matrixInverse <<- matInverse
  }
  getInverse <- function() {
    matrixInverse
  }
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

#' Computes the inverse (denoted by X') of an invertible matrix (denoted by X). 
#' @param x A list of functions provided by the makeCacheMatrix function.
#' @return Inverse of a matrix X.
cacheSolve <- function(x, ...) {
  matrixInverse <- x$getInverse()
  if(is.null(matrixInverse)) {
    matrixInverse <- solve(x$getMatrix())
    x$setInverse(matrixInverse)
  }
  matrixInverse
}