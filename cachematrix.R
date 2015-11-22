##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve retrieves the inverse from the cache.


## The function, makeCacheMatrix creates a special "matrix", which is really a list containing
##functions to set and get the value of the matrix and cache the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set_mat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get_mat <- function() x
  setinvmat <- function(invmat) inv <<- invmat
  getinvmat <- function() inv
  list(set_mat = set_mat, get_mat = get_mat,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
  
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), the cachesolve
##retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<- x$getinvmat()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get_mat()
  inv <- solve(data)
  x$setinvmat(inv)
  inv
}
