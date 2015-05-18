## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. Following functions are designed to provide inverse matrix 
## caching functionality, by introducing matrix wrapper what are able to store original matrix
## as well as cache of inverse matrix after it has been calculated once 

## This function creates a wrapper around original matrix, and return a list of functions to 
## get/set original matrix value and get/set inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrixCache <- NULL
  set<- function(m){
    x <<- m
    inverseMatrixCache <<-NULL # we need to dicard cache each time we change the original matrix
  }
  get <-function(){
    x
  }
  setInverse <-function(inverseMatrix){
    inverseMatrixCache <<- inverseMatrix
  }
  getInverse <-function(){
    inverseMatrixCache
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


# The following function returns the inverse of the matrix (it assumes that the matrix is always invertible).
# Matrix should be represented as matrix wrapper created by makeCacheMatrix function.  cacheSolve function first returns 
# already computed value if it exists. If not, it computes the inverse, and sets the value 
# in the cache via setinverse function.


cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data.")
    return(inverseMatrix)
  }
  originalMatrix <- x$get()
  inverseMatrix <- solve(originalMatrix)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
