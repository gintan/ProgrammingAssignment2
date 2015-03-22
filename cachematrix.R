## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly 
## This code consists of a pair of functions 
## that cache the inverse of a matrix
## 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	c <- NULL
	set <- function(y) {
	  # cache the inputed matrix
	  x <<- y 
	  c <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) c <<- inverse
	getinverse <- function() c
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	c <- x$getinverse()
	
	if(!is.null(c)){
	## If the inverse has already been calculated and not changed, then return x
	  message("getting cached data")  
	  return(c)
	  
	}	
	## If the inverse has not been calculated, computing the inverse of a square matrix using solve function
	y <- x$get()
	c <- solve(y, ...)
	x$setinverse(c)
	c
}
