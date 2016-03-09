## The functions cache the inverse of a matrix to reduce the high cost
## computation of inversion. Note that the matrix you build 
## must always be invertible.


## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {
      invM <- NULL
      set <- function(Y) {
            X <<- Y
            invM <<- NULL
      }
      get <- function() X
      setInvM <- function(inverseMatrix) invM <<- inverseMatrix
      getInvM <- function() invM
      list(set = set, get = get,
           setInvM = setInvM,
           getInvM = getInvM)
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse of the matrix is cached (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## Return InvX: matrix that is the inverse of the matrix X.

cacheSolve <- function(X, ...) {
      invM<- X$getInvM()
      if(!is.null(invM)) {
            message("getting cached data")
            return(invM)
      }
      data <- X$get()
      InvM <- solve(data)
      X$setInvM(invM)
      InvM
}
