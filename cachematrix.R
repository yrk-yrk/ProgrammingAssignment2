## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The makeCacheMatrix function creates a matrix of the special type.
#It basically returns a list which has four functions associated with it,
# one to return data(matrix in this case),
# one to set data(matrix in this case)
# one to return inverse
# and the last one to set inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	#This function sets the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
	#This function returns the matrix.
        get <- function() x
	
	#This function sets the inverse of the matrix.
        setinverse <- function(calculated_inverse) inverse <<- calculated_inverse
        
	#This function returns the inverse of the matrix.
	getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

#This function tries to lookup the value of inverse of the matrix.
#if it is found in the cache, it returns it,
#otherwise it will calculate it and store it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	#Gets the value stored in the inverse of the matrix.	
	inv <- x$getinverse()
	
	#Checks if it exists, if it exists, then it will return it rightaway.
	if(!is.null(inv)) {
		message("getting cached data")
                return(inv)			
	}
	
	#Gets the data.
	data <- x$get()

	#Calculates the inverse.
	calculated_inv <- solve(data);
	
	#Sets the values of inverse in the cache.
	x$setinverse(calculated_inv);

	#Returns the value.
	calculated_inv
}
