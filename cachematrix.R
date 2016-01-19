# These functions will take an invertible matrix and calculate the inverse of that matrix.
# There are two functions in this program, a cache function and a solve function.
# The first function will cache any given matrix and the solutions of the inverse of the matrix.
# The second matrix will check if there is any matrix data that was cached by the first function.
# If data does exist, the second function will also solve the inverse of the given invertible matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric){
        invMatrix <- NULL
		
        set <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
		
        get <- function() x
		
		setInverse <- function(inverse) invMatrix <<- inverse
		getInverse <- function() invMatrix
		
		list(set = set, get = get,
			setInverse = setInverse,
			getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
		invMatrix <- x$getInverse()
		
		if(!is.null(invMatrix)){
				message("Getting cached data...")
				return(invMatrix)
		}
		
		data <- x$get()
		invMatrix <- solve(data, ...)
		
		x$setInverse(invMatrix)
		invMatrix
}