## set function sets the value of the matrix
## get function gets the value of the matrix
## setinv function sets the inverse matrix
## getinv function gets the inverse matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
	set <- function (y) {
		x <<- y
		inv <<- NULL		
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## if inverse matrix has already been calculated and the matrix has not changed then cacheSolve gets the inverse from cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()## Return a matrix that is the inverse of 'x'
        if (!is.null(inv)) {
		message("getting cached inversed matrix")
		return(inv)
	} else {
		inv <- solve(x$get())
		x$setinv(inv)
		return(inv)
}
