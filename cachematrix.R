
## Function that creates a cacheable matrix, which 
## has functions to set the inverse function of the matrix,
## get the inverse function of the matrix
## get the inverse matrix and set the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
		    x <<- y
		    m <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		list(set = set, get = get,
		     setinverse = setinverse,
		     getinverse = getinverse)

}


# Returns the inverse of matrix x.
# If the inverse is already in the cache, no inverse computation 
# is performed.
# Otherwise, the inverse is computed, stored in the cache and 
# returned.

cacheSolve <- function(x, ...) {

	   m <- x$getinverse()
	   if(!is.null(m)) {
	   		   message("getting cached data")
			   return(m)
	   }
	   data <- x$get()
	   m <- solve(data,...)
	   x$setinverse(m)
	   m
}
