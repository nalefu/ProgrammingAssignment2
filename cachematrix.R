## "makeCacheMatrix" and "cacheSolve" are used to cache the inverse of a matrix

makeCacheMatrix <- function(z = matrix()) {
   ## setting the value of the matrix
    y <- NULL 
    x <- NULL 
    setmatrix <- function(y) { 
        z <<- x
        y <<- NULL 
    }
    ## getting the value of the matrix
    getmatrix <- function() 
        z 
    ## setting the value of the matrix
    setinverse <- function(solve) 
        y <<- solve 
    getinverse <- function() 
        y 
    ##getting the inverse of the matrix
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         setinverse = setinverse,
         getinverse = getinverse)
}

## "cacheSolve" returns the inverse of a matrix. The function checks if the inverse has already been computed and if it hasn't been then it computes the inverse.  
cacheSolve <- function (z=matrix(), ...) {
    y <- z$getinverse() 
    if(!is.null(y)){ 
        if(z$setmatrix() == z$getmatrix()) { 
            message("getting cached data")
            y <- z$getinverse()
        }
        return(y)
    }