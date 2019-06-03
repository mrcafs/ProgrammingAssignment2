## The following functions create a Matrix object, from a square matrix passed to argument x,
## and cache it into memory, so it can re-used without having to be calculated again, to get
## the inverse of that matrix.


##	This function creates a square "matrix" object and caches it into memory.

makeCacheMatrix <- function(x = matrix()) {
        
		m <- NULL

		set <- function(y) {
                x <<- y
                m <<- NULL
        }
      
		get <- function() x
        
		setinverse <- function(solve) m <<- solve
        
		getinverse <- function() m
        
		list(	set = set,
				get = get,
				setinverse = setinverse,
				getinverse = getinverse)
}


##	This function calculates the inverse of an object created using makeCacheMatrix.
##	If the inverse has already been calculated, then it is retrieved from the cache.

cacheSolve <- function(x, ...) {
        
		m <- x$getinverse()
        
		if(!is.null(m)) {
                
				message("getting cached data")
                
				return(m)
        }
        
		data <- x$get()
        
		m <- solve(data, ...)
        
		x$setinverse(m)
        
		m
}



