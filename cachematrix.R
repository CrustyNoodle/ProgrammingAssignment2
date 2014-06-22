## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix - function(x = matrix())
##  Purpose -   to initialise the matrix by saving the matrix data
##              and defining the setter and getter functions for 
##              working with the matrix
##
## cacheSolve - function(x, ...)
##  Purpose -   to return the inverse of the matrix.
##              If the inverse is yet to be calculated for this matrix
##              it will be calculated and then the result cached for later
##              use.  If the inverse of the matrix has already been 
##              caluculated for this matrix then the cached result is
##              returned.

####################################################################
## makeCacheMatrix - function(x = matrix())
##  Purpose -   to initialise the matrix by saving the matrix data
##              and defining the setter and getter functions for 
##              working with the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## set the inverse variable to null to indicate that no inverse is cached
    
    ## Define the "setter" function - caches the matrix
    set <- function(y) {
        x <<- y         ## cache the matrix
        inv <<- NULL    ## set the cached inverse to null
    }
    
    ##  Define the "getter" function - returns the original matrix
    get <- function() x
    
    ## Define the setInverse function - caches the result of the inverse operation
    setInverse <- function(inverse) inv <<- inverse
    
    ## Define the getInverse function - returne the cached inverse
    getInverse <- function() inv
    
    ## Return a list representing the defined functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


####################################################################
## cacheSolve - function(x, ...)
##  Purpose -   to calculate the matrix inverse and cache the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        ## message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}


###################################################################
## function to test the operation of the inverse caching functions
testMakeCacheMatrix <- function(){
    
    ## generate the test matrix
    myMat <- matrix(runif(9, 0, 100),3,3)
    print(myMat)
    
    ## put this into our cached matrix object
    cachedMatObj <- makeCacheMatrix(myMat)
    
    ## test the cached inverse result (before calculating the inverse)
    print(cachedMatObj$get())
    print(cachedMatObj$getInverse())
    
    ## Try the cacheSolve function
    print(cacheSolve(cachedMatObj))
    
    ## now retry the cached inverse
    print(cachedMatObj$getInverse())
    
    ## now run a speed comparison between the un-cached matrix and the cached one
    print('Uncached matrix inverse')
    print(
        system.time(
            for(i in 1:10000)
            {
                solve(myMat)+1;
            }
        )
    )
    
    print('Uncached matrix inverse')
    print(
        system.time(
            for(i in 1:10000)
            {
                cacheSolve(cachedMatObj)+1;
            }
        )
    )

}
