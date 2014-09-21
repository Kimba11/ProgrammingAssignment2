## These functions are intended to speed up the computation of matrix inversions
## my minimizing the number of times the matrix inversion is computed.
## They take advantage of an R function capability to assign values outside of it's 
## calling environment.
## The function makeCacheMatix() generates a special matrix that can cache its inverse



makeCacheMatrix <- function(x = matrix()) {

    #set variable x (matrix invert in this case) to NULL
    x <- NULL
    
    #set function sets x to the argument y and set m to null
    #
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
}


## cacheSolve() generates the inverse of a matrix x 
## if the inverse exists and still valid it uses the cached version, otherwise it 
## calculates the invert using R's solve() function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	## Checks to see if the previously calculated matix inverse is still valid
	## and if TRUE uses the cached value
		
    x <- x$makeCacheMatrix()
    
    ## if not null, a valued was cached, so return x
    if(!is.null(x)) {
        message("getting cached data")
        return(x)
    }
    
    ## since its null, set data to x from makeCacheMatrix
    
    data <- x$get()
    
    ## calculate the matrix inversion using R's solve() function
    x <- solve(data, ...)
    ## return the matrix inversion
    x
}


