## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object which can hold it's inverse matrix value
makeCacheMatrix <- function(x = matrix()) {
  # the inverse matrix is initialized as null
  inverse_matrix <- NULL
  
  # setting the matrix invalidates any prior inverse matrix
  # (hence it's reset to NULL)
  setMatrix <- function(original_matrix) {
    x <<- original_matrix
    inverse_matrix <<- NULL
  }
  
  # get the matrix
  getMatrix <- function() {
    x
  }
  
  # set the inverste matrix
  setInverse <- function(i_m) {
    inverse_matrix <<- i_m
  }
  
  # get the inverse matrix
  getInverse <- function() {
    inverse_matrix
  }
  
  # the list of exposed 'methods'
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated, the cached value is returned
# if the inverse is NULL, it is calculated, stored and returned
cacheSolve <- function(x, ...) {
  
  # if the inverse matrix is null, calculate it and store it
  if (is.null(x$getInverse())) {
    x$setInverse(solve(x$getMatrix()))
  }
  
  # inverse is now calculated, so it can be returned
  x$getInverse()
}

