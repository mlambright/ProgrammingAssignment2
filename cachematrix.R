## These functions together solve for the inverse
## of an arbitrary matrix, and cache the solution
## to conserve processing resources in future
## calculations

## This function returns a list of functions which
## cache the solution of a matrix

makeCacheMatrix <- function(x = matrix()) {
  solution <- NULL
  set <- function(y) {
    x <<- y
    solution <<- NULL
  }
  get <- function() x
  setSolution <- function(matrixSolution) solution <<- matrixSolution
  getSolution <- function() solution
  
  list(set = set, get = get,
       setSolution = setSolution,
       getSolution = getSolution)  
}


## This function gets and sets the objects created in the
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  solution = x$getSolution()
  if (!is.null(solution)) {
    message("getting cached data")
    return(solution)
  }
  data <- x$get()
  solution <- solve(data, ...)
  x$setSolution(solution)
  solution
}
