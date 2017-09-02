## Cache handling functions for invertible matrices

## it takes a invertible matrix and returns
## a special matrix object which has the 
## capability to cache the result rather than
## calculate all the time 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## It expects the special matrix object 
## from makeCacheMatrix and calulates
## it invertible if not already
## available in the special matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(is.matrix(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
