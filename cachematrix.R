# File:		cachematrix.R
# Description:	Contains matrix related functions including:
#			makeCacheMatrix(matrix)
#			cacheSolve('makeCacheMatrix list output')
			

# makeCacheMatrix takes an invertible matrix and generates a list of four discrete 
# functions associated with it.  It allows the user to (1) set the cached matrix 
# and then, (2) retrieve it.  Next, (3) the inverse of the matrix can be computed 
# and cached, or if already computed and cached, it will be retrieved from the cache 
# for faster operation.  Finally, (4) the user can retrieve the cached inverse. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {	# (1)
		x <<- y
		m <<- NULL
	}
	get <- function() x		# (2)
	setinverse <- function(cacheSolve) m <<- cacheSolve	# (3)
	getinverse <- function() m						# (4)
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	# Create and return list of functions
}

# cacheSolve will retrieve the computed inverse of a makeCacheMatrix controlled 
# matrix.  If no cached inverse is available, the inverse will be computed and 
# cached through the makeCacheMatrix function list.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()		# Retieve cached inverse matrix object
	if(!is.null(m)) {		# If the cached inverse matrix exists, return it.
		message("getting cached matrix")
		return(m)
	}
	data <- x$get()		# Retrieve primary matrix
	m <- solve(data, ...)	# Compute its inverse matrix
	x$setinverse(m)		# Cache the inverse matrix
	m					# Return inverse matrix
}
