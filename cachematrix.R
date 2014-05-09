## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse (using R's solve function)

## This function creates a special "matrix", which is really a list
## containing a function to:
##   - set -> it sets the value of the matrix
##   - get -> it gets the value of the matrix
##   - setsolve -> it sets the value of the solve (inverse of the matrix)
##   - getsolve -> it gets the value of the solve (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## This function calculates the inverse of the matrix.
## It first checks if the solve (inverse of the matrix) has already been
## calculated.
##   - If so, it gets the solve from the cache and skips computation.
##   - Otherwise, it calculates the inverse of the matrix and sets it via
##     the setsolve function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
