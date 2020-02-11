## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matrix_cache <- NULL
        set <- function(y) {
                x <<- y
                matrix_cache <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrix_cache <<- inverse
        getinverse <- function() matrix_cache 
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_cache <- x$getinverse()
        if(!is.null(matrix_cache)) {
                message("getting cached data")
                return(matrix_cache)
        }
        data <- x$get()
        matrix_cache <- solve(data, ...)
        x$setinverse(matrix_cache)
        matrix_cache
}
