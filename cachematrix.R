## These functions will:
## - first create a "Cache matrix" via "makeCacheMatrix" with all information 
## on the matrix itself, its inverse (if it has already been computed, 
## otherwise the value is set to NULL) and the information on the function
## to use in the inversion;
## - then, "cacheSolve" will compute the actual inverse of the matrix only if 
## the inverse matrix has not been computed yet. 

## Definition of the "Cache Matrix"

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) xinv <<- ginv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computation of the inverse of a matrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # the code is written in such a way that if 
  # the inverse of x has already been computed 
  # it just avoids another computation and takes
  # back the already computed value. 
  
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- ginv(data, ...)
  x$setinv(xinv)
  xinv
}
