## This pair of functions is intended to speed up the computation of matrix operations
## by minimizing the number of times the actual matrix inversion is computed.
## It takes advantage of an R function's ability to assign values outside of its 
## calling environment.

## The function makeCacheMatix() generates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    #set variable x (matrix in this case) to NULL
    x <- NULL
    
    ## set function creates the matrix inverse, seting x to the argument y and 
    ## and setting m to null (flag variable)
    
    set <- function(y) {
        x <<- solve(y)
        m <<- NULL
    }
    
    ## get returns the value of x 
    get <- function() {
    	x
    	} 

}


## cacheSolve() generates the inverse of a matrix x.  If the inverse exists
## and still valid, the function uses the cached version, otherwise it 
## calculates the invert using R's solve() function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	## Checks to see if the previously calculated matix inverse is still valid
	## and if TRUE uses the cached value
    x <- x$makeCacheMatrix()
    
    ## if not null, a valued was cached, so return x
    if(!is.null(x)) {
    	message("using cached data")
        return(x)
    }
    else {
    	## since its null, set data to x from makeCacheMatrix
    	data <- x$get()
    
   	 	## calculate the matrix inversion using R's solve() function
    	x <- solve(data, ...)
    
    	## return the calculated matrix inversion
    	return(x)
    	}
}