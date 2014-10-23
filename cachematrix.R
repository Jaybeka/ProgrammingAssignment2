## Put comments here that give an overall description of what your
## functions do

## create an special "matrix" object that stores the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- inv
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## compute the inverse of the special "matrix" returned by `makeCacheMatrix` function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
    	message("getting cached data")
    	return(inv)
    }
    data <- x$get()
    inv <- solve(x, ...)
    x$setinv(inv)
    inv
}
