## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## These are a pair of functions that cache the inverse of a matrix, assuming 
## that the input matrix is always invertible.

# Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}


# Computes the inverse of the special matrix returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) { 
    message("Getting cached data...")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
