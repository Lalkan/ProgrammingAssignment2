## This function creates a special matrix object that possesses the capacity to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(my_inv) inv <<- my_inv
  get_inverse <- function() inv
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## This function computes the inverse if it has not yet been cached and otherwise, just returns the stored value of the inverse

cacheSolve <- function(x, ...) {
  ## Return the inverse of x
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$set_inverse(inv)
  inv
}