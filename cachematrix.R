## Programming Assignment 2
## Creates a data structure for calculating and caching
## matrix inverses

## Creates a data structure for caching matrix inverses

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Solves for matrix inverse. Pulls from cache if existant

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if (!is.null(i)) {
		message("getting cached data")
		return (i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}
