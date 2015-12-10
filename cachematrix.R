## Functions to create a specialized matrix that is capable of caching
## already calculated results, and a solve function that uses a cached result
## when available

## Returns a list of functions for a cached matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setinverse <- function (solve) m <<- solve
  getinverse <- function () m
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Returns the cached value (if available) or calculates the inverse of a 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (!is.null(m <- x$getinverse())) {
    return (m)
  }
  m <- x$setinverse(solve(x$get(), ...))
  m
}
