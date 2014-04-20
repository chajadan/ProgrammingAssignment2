## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix exposes a list of functions (set, get, setinverse, getinverse)
# that allows the user to manipulate a closure that contains a matrix (by get,set) and its
# cached inverse. The argument x is assumed to be invertible.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve takes the closure of makeCacheMatrix and returns the inverse of
# the cached matrix, either calculating that inverse if necessary or returning
# the previously cached results
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
