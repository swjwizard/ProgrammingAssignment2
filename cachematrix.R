###################################################
##
##	cachematrix.R
##	Authtor:  Stephen Jones
##	
###################################################


##  These functions create a Matrix then return tine inverse
##  of that matrix.  The first funcion creates and returns a matrix.
##  The second function returns the inverse of the matrix created in
##  the first but will look to see if the matrix is cached in memory
##  first.  If the matrix is not found in memory, it places it in cache.
## 
##  NOTE: I spent a lot of time on this.  After much consternation,
##  I found that the solution was simple or I just missed the boat
##  entirely.
##   

##
##  The function, makeCacheMatrix, creates a matrix from parameters
##  given by the function cacheSolve.  This function creates a list
##  operations that can be peformed on the matrix.
##

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set = set, get = get,
	     setmatrix = setmatrix,
	     getmatrix = getmatrix)
}

##
##  The function, cacheSolve, looks for an inverted matrix in cache.
##  If it finds it, it returns the inverse of matrix.  If the matrix
##  is not in cache, this function sends the parameters to create a
##  matrix to makeCacheMatrix to get the matrix created in cache. Then
##  this matrix returns the inverse of the matrix.
##

cacheSolve <- function(x = matrix(), ...) {
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
}


