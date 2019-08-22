## Caches a matrix calculation so that the results can be 
## retrieved without having to re-calculate

## Creates a matrix object so that its value can be cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Computes an inverse of a matrix, then caches for future use

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  # if checks if the inverse has been calculated previously
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

## Example Usage
## xyz <- matrix(c(1,2,3,4),2,2)
## xyz2 <- makeCacheMatrix(xyz)
## cacheSolve(xyz2)
