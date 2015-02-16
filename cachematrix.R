makeCacheMatrix <- function(z = matrix()) {
    y <- NULL 
    x <- NULL 
    setmatrix <- function(y) { 
        z <<- x
        y <<- NULL 
    }
    getmatrix <- function() 
        z 
    setinverse <- function(solve) 
        y <<- solve 
    getinverse <- function() 
        y 
    list(setmatrix = setmatrix, getmatrix = getmatrix, 
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function (z=matrix(), ...) {
    y <- z$getinverse() 
    if(!is.null(y)){ 
        if(z$setmatrix() == z$getmatrix()) { 
            message("getting cached data")
            y <- z$getinverse()
        }
        return(y)
    }