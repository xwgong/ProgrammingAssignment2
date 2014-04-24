## makeCacheMatrix() create a Matrix object, including a set of functions
## cacheSolve() calculate the inverse of the matrix created by the above function


## This function creates a special matrix, including a list containing a function to 
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- inverse
		getinv <- function() inv
		list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## This function calculates the inverse of the matrix created by the above function. 
## It first checks whether the inverse has already been calculated. If so, it gets the inverse from the cache and skips the calculation
## otherwise, it calculates the inverse of the matrix and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if (!is.null(inv)) {
				message("getting cached inverse")
				return(inv)
		}
		
		data <- x$get()
		inv <- solve(data,...)
		x$setinv(inv)
		inv
}
